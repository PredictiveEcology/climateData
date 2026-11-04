## Does a new ClimateNA release reproduce the stored outputs of one archive type?

## ClimateNA's period argument and variable set for an existing period folder.
pipeline_folder_arg <- function(type, folder) {
  msy <- regmatches(folder, regexpr("(MSY|M|S|Y)$", folder))
  stem <- sub("(MSY|M|S|Y)$", "", folder)
  ext <- switch(type, historical = ".ann", historical_normals = ".nrm", ".gcm")
  list(arg = paste0(stem, ext), msy = msy)
}

## Rerun a sample of the stored periods this release would keep (made by an older ClimateNA
## version, but not older than min_version) and compare them with the stored outputs: tiles 6,
## 39 and 77 when present, otherwise the first and last tile; each tile's earliest and latest
## period. "compatible" when there is no such output, so raising min_version (which regenerates
## the older periods) lets the pipeline go on. Stops on "changed": raising min_version for this
## type in release.yml is the maintainer's decision.
compat_check <- function(
  type,
  climatenaExe,
  climatenaVersion,
  minVersion,
  image,
  climatenaDir,
  demDir,
  outRoot,
  uid,
  gid,
  timeout = 3600
) {
  pipeline_load()
  stamps <- list.files(
    file.path(outRoot, "raw", type),
    pattern = "[.]stamp$",
    recursive = TRUE,
    full.names = TRUE
  )
  versions <- numeric_version(vapply(
    stamps,
    \(s) read.dcf(s)[1, "climatena_version"],
    character(1)
  ))
  older <- stamps[
    versions < numeric_version(climatenaVersion) & versions >= numeric_version(minVersion)
  ]
  if (length(older) == 0) {
    return("compatible")
  }

  tile <- basename(dirname(older))
  pick <- intersect(c("6", "39", "77"), tile)
  if (length(pick) == 0) {
    pick <- unique(c(tile[1], tile[length(tile)]))
  }
  sample <- unlist(lapply(pick, function(t) {
    s <- sort(older[tile == t])
    unique(c(s[1], s[length(s)]))
  }))

  scratchRoot <- file.path(outRoot, "compat", climatenaVersion, type)
  imageDigest <- pipeline_image_digest(image)
  comparisons <- lapply(sample, function(stamp) {
    tileDir <- dirname(stamp)
    folder <- sub("[.]stamp$", "", basename(stamp))
    a <- pipeline_folder_arg(type, folder)
    scratch <- file.path(scratchRoot, basename(tileDir))
    dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
    name <- paste0(
      "cna-compat-",
      gsub("[^A-Za-z0-9_.-]", "-", paste(type, basename(tileDir), folder))
    )
    args <- .climatenaDockerArgs(
      name = name,
      image = image,
      climatenaDir = climatenaDir,
      demFile = pipeline_dem_file(demDir, basename(tileDir)),
      outDir = scratch,
      exe = climatenaExe,
      msy = a$msy,
      periodArg = a$arg,
      uid = uid,
      gid = gid
    )
    log <- file.path(scratchRoot, "compat.log")
    if (!pipeline_run_climatena(args, name, log, timeout)) {
      stop("ClimateNA ", climatenaVersion, " failed for the compatibility sample ", folder)
    }
    .compareFolders(file.path(scratch, folder), file.path(tileDir, folder))
  })
  verdict <- .compatVerdict(do.call(rbind, comparisons))
  if (verdict == "changed") {
    stop(
      "ClimateNA ",
      climatenaVersion,
      " changes ",
      type,
      " outputs; set min_version: ",
      type,
      ': "',
      climatenaVersion,
      '" in release.yml to regenerate'
    )
  }
  verdict
}
