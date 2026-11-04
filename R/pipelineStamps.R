## Period stamps for the data pipeline (data-raw/pipeline/).
##
## Each ClimateNA output folder gets a sibling `<folder>.stamp` file (DCF) recording the
## ClimateNA version, the DEM tile's hash and the Docker image digest that produced it. The stamp
## sits next to the folder, not inside it, so it never ends up in a zip.

.stampPath <- function(tileDir, folder) {
  file.path(tileDir, paste0(folder, ".stamp"))
}

.writeStamp <- function(tileDir, folder, climatenaVersion, demHash, image) {
  path <- .stampPath(tileDir, folder)
  write.dcf(
    data.frame(
      climatena_version = climatenaVersion,
      dem_hash = demHash,
      image = image,
      time = as.numeric(Sys.time())
    ),
    path
  )
  invisible(path)
}

.readStamp <- function(tileDir, folder) {
  path <- .stampPath(tileDir, folder)
  if (!file.exists(path)) {
    return(NULL)
  }
  as.list(read.dcf(path)[1, ])
}

## A period is done when its folder passes the archive check and its stamp shows a ClimateNA
## version at or above `minVersion` and the same DEM. Anything else is rerun.
.periodDone <- function(tileDir, folder, expected, minVersion, demHash) {
  stamp <- .readStamp(tileDir, folder)
  if (is.null(stamp)) {
    return(FALSE)
  }
  if (numeric_version(stamp$climatena_version) < numeric_version(minVersion)) {
    return(FALSE)
  }
  if (!identical(stamp$dem_hash, demHash)) {
    return(FALSE)
  }
  listing <- .dirListing(tileDir, folder)
  e <- .archiveExpectedByVersion(listing, expected)$expected
  .archiveComplete(.archiveCheck(listing, e))
}
