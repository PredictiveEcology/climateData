#' @keywords internal
.lonlat <- "epsg:4326"

## Stop with the missing folders named, instead of "all(dir.exists(allDirs)) is not TRUE".
## A folder is missing when the downloaded archive does not contain it -- e.g., the
## 2010s CNRM-ESM2-1 ssp370 archive for tile 39 has no 2013-2016.
.stopIfMissingClimateDirs <- function(allDirs) {
  missingDirs <- allDirs[!dir.exists(allDirs)]
  if (length(missingDirs) > 0) {
    stop(
      "Climate data folders are missing for tile ",
      paste(unique(basename(dirname(missingDirs))), collapse = ", "),
      "; the downloaded archive may not contain them:\n  ",
      paste(missingDirs, collapse = "\n  "),
      call. = FALSE
    )
  }
  invisible(allDirs)
}
