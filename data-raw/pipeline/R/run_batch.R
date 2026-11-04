## Running ClimateNA for one batch, in the climatena-wine Docker image.

## Raw outputs: <outRoot>/raw/<type>[/<gcm>_ssp<ssp>]/<tile>/<period folder>, with a sibling
## <period folder>.stamp for each finished period.
pipeline_tile_dir <- function(outRoot, spec) {
  scen <- if (is.na(spec$gcm)) character(0) else paste0(spec$gcm, "_ssp", spec$ssp)
  do.call(file.path, as.list(c(outRoot, "raw", spec$type, scen, spec$tile)))
}

pipeline_dem_file <- function(demDir, tile) {
  file.path(demDir, paste0("can_dem_", tile, ".asc"))
}

## The image's registry digest when it has one (pulled from ghcr.io), otherwise its local ID.
pipeline_image_digest <- function(image) {
  d <- suppressWarnings(system2(
    "docker",
    c("image", "inspect", "--format", "{{index .RepoDigests 0}}", image),
    stdout = TRUE,
    stderr = FALSE
  ))
  if (length(d) == 0 || !nzchar(d[1]) || !is.null(attr(d, "status"))) {
    d <- system2("docker", c("image", "inspect", "--format", "{{.Id}}", image), stdout = TRUE)
  }
  d[1]
}

## Run ClimateNA once in a container; TRUE on a clean exit. A timed-out run's container (which
## this function started and named) is removed, so it can't keep running in the background.
pipeline_run_climatena <- function(args, name, log, timeout) {
  rc <- suppressWarnings(system2("docker", args, stdout = log, stderr = log, timeout = timeout))
  if (identical(as.integer(rc), 124L)) {
    system2("docker", c("rm", "-f", name), stdout = FALSE, stderr = FALSE)
    cat("timed out after", timeout, "s:", name, "\n", file = log, append = TRUE)
  }
  identical(as.integer(rc), 0L)
}

## Run every period of a batch that isn't already done, check each output, stamp it, and write
## the batch's manifest. Stops (so the target errors and is retried next time) naming any
## periods that failed.
run_batch <- function(
  spec,
  pkgDir,
  climatenaExe,
  climatenaVersion,
  image,
  climatenaDir,
  demDir,
  outRoot,
  minVersion,
  uid,
  gid,
  timeout = 3600
) {
  pipeline_load(pkgDir)
  tileDir <- pipeline_tile_dir(outRoot, spec)
  dir.create(tileDir, recursive = TRUE, showWarnings = FALSE)
  logDir <- file.path(outRoot, "logs")
  dir.create(logDir, showWarnings = FALSE)
  log <- file.path(logDir, paste0(spec$id, ".log"))
  demFile <- pipeline_dem_file(demDir, spec$tile)
  demHash <- digest::digest(demFile, file = TRUE, algo = "xxhash64")
  normals <- spec$type %in% c("historical_normals", "future_normals")
  vars <- if (normals) .allowedClimateVars_nrm else .allowedClimateVars_MSY

  imageDigest <- NULL
  failed <- character(0)
  for (p in spec$periods) {
    folder <- .periodFolder(spec$type, p, spec$msy, spec$gcm, spec$ssp)
    expected <- file.path(folder, paste0(vars, ".asc"))
    if (.periodDone(tileDir, folder, expected, minVersion, demHash)) {
      next
    }
    if (is.null(imageDigest)) {
      imageDigest <- pipeline_image_digest(image)
      Sys.sleep(stats::runif(1, 0, 30)) # stagger container starts across batches
    }
    unlink(file.path(tileDir, folder), recursive = TRUE) # clear any partial output
    unlink(.stampPath(tileDir, folder))
    name <- paste0("cna-", gsub("[^A-Za-z0-9_.-]", "-", paste(spec$id, p, sep = "-")))
    args <- .climatenaDockerArgs(
      name = name,
      image = image,
      climatenaDir = climatenaDir,
      demFile = demFile,
      outDir = tileDir,
      exe = climatenaExe,
      msy = spec$msy,
      periodArg = .periodArg(spec$type, p, spec$gcm, spec$ssp),
      uid = uid,
      gid = gid
    )
    ok <- pipeline_run_climatena(args, name, log, timeout) &&
      .archiveComplete(.archiveCheck(.dirListing(tileDir, folder), expected))
    if (ok) {
      .writeStamp(tileDir, folder, climatenaVersion, demHash, imageDigest)
    } else {
      failed <- c(failed, folder)
    }
  }
  if (length(failed) > 0) {
    stop(
      "ClimateNA failed or wrote incomplete output for ",
      spec$id,
      ": ",
      paste(failed, collapse = ", ")
    )
  }

  folders <- .periodFolder(spec$type, spec$periods, spec$msy, spec$gcm, spec$ssp)
  manifest <- data.frame(
    folder = folders,
    n_files = vapply(folders, \(f) length(list.files(file.path(tileDir, f))), integer(1)),
    climatena_version = vapply(folders, \(f) .readStamp(tileDir, f)$climatena_version, character(1))
  )
  out <- file.path(outRoot, "raw", paste0(spec$id, ".tsv"))
  utils::write.table(manifest, out, sep = "\t", quote = FALSE, row.names = FALSE)
  out
}
