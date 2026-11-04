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

## A node-local directory for ClimateNA to write into. Writing straight to an NFS share is very
## slow: ClimateNA checks its open files as it writes, which forces every write out to the
## server, and 90 such runs at once stall in I/O wait. CLIMATENA_WORK_DIR when set (local crew
## workers inherit it from the main process), otherwise the worker's tempdir().
pipeline_work_dir <- function() {
  d <- Sys.getenv("CLIMATENA_WORK_DIR")
  if (nzchar(d)) d else tempdir()
}

## Copy an output folder from the work directory into `to` (a tile's raw folder), keeping
## modification times, and check that the copy has the same files with the same sizes.
pipeline_copy_folder <- function(from, to, folder) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(file.path(from, folder), to, recursive = TRUE, copy.date = TRUE)
  m <- merge(.dirListing(from, folder), .dirListing(to, folder), by = "path", all = TRUE)
  isTRUE(ok) && nrow(m) > 0 && !anyNA(m) && all(m$size.x == m$size.y)
}

## Append a run's log to a batch log, and remove it.
pipeline_append_log <- function(log, runLog) {
  if (file.exists(runLog)) {
    file.append(log, runLog)
    unlink(runLog)
  }
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
## the batch's manifest. Each run writes to the node-local work directory; its output is checked
## there, copied to the tile's raw folder, and stamped once the copy is checked. Stops (so the
## target errors and is retried next time) naming any periods that failed. The target runs on
## every tar_make() (cue "always"), so a damaged or deleted output folder is found and rerun;
## finished periods cost only a listing.
run_batch <- function(
  spec,
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
  pipeline_load()
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
  workDir <- file.path(pipeline_work_dir(), spec$id)
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
    unlink(.stampPath(tileDir, folder)) # no stamp until a checked copy is in place
    unlink(file.path(tileDir, folder), recursive = TRUE) # clear any old or partial output
    unlink(file.path(workDir, folder), recursive = TRUE)
    dir.create(workDir, recursive = TRUE, showWarnings = FALSE)
    runLog <- file.path(workDir, paste0(folder, ".log"))
    name <- paste0("cna-", gsub("[^A-Za-z0-9_.-]", "-", paste(spec$id, p, sep = "-")))
    args <- .climatenaDockerArgs(
      name = name,
      image = image,
      climatenaDir = climatenaDir,
      demFile = demFile,
      outDir = workDir,
      exe = climatenaExe,
      msy = spec$msy,
      periodArg = .periodArg(spec$type, p, spec$gcm, spec$ssp),
      uid = uid,
      gid = gid
    )
    ok <- pipeline_run_climatena(args, name, runLog, timeout) &&
      .archiveComplete(.archiveCheck(.dirListing(workDir, folder), expected)) &&
      pipeline_copy_folder(workDir, tileDir, folder)
    pipeline_append_log(log, runLog)
    unlink(file.path(workDir, folder), recursive = TRUE)
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

  ## the stamp fields make any regenerated period change the manifest, so its zips are recut
  folders <- .periodFolder(spec$type, spec$periods, spec$msy, spec$gcm, spec$ssp)
  stamps <- lapply(folders, \(f) .readStamp(tileDir, f))
  manifest <- data.frame(
    folder = folders,
    n_files = vapply(folders, \(f) length(list.files(file.path(tileDir, f))), integer(1)),
    climatena_version = vapply(stamps, `[[`, character(1), "climatena_version"),
    dem_hash = vapply(stamps, `[[`, character(1), "dem_hash"),
    time = vapply(stamps, `[[`, character(1), "time")
  )
  out <- file.path(outRoot, "raw", paste0(spec$id, ".tsv"))
  utils::write.table(manifest, out, sep = "\t", quote = FALSE, row.names = FALSE)
  out
}
