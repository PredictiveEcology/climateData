## Period stamps for the data pipeline (data-raw/pipeline/).
##
## Each stored ClimateNA output (`<folder>.zip`) gets a sibling `<folder>.stamp` file (DCF)
## recording the ClimateNA version, the DEM tile's hash and the Docker image digest that produced
## it. The stamp sits next to the zip, not inside it, so it never ends up in a published zip.

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

## Each finished period is stored as one zip, `<folder>.zip` (holding `<folder>/<var>.asc`), next
## to its stamp: a single file per period keeps the raw tree small and cheap on a network share.
.periodZip <- function(tileDir, folder) {
  file.path(tileDir, paste0(folder, ".zip"))
}

## A period is done when its stored zip passes the archive check and its stamp shows a ClimateNA
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
  zip <- .periodZip(tileDir, folder)
  if (!file.exists(zip)) {
    return(FALSE)
  }
  listing <- .zipListing(zip)
  e <- .archiveExpectedByVersion(listing, expected)$expected
  .archiveComplete(.archiveCheck(listing, e))
}
