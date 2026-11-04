#' @keywords internal
.lonlat <- "epsg:4326"

## ClimateNA v7.60 renamed its radiation variables (Rad*, MAR -> rsds*; see
## .legacyRadiationNames). Stop before any download when an old name is requested.
.stopIfLegacyRadiation <- function(vars) {
  base <- sub("_normal$", "", sub("^(historical|future|projected)_", "", vars))
  old <- base %in% .legacyRadiationNames
  if (any(old)) {
    new <- names(.legacyRadiationNames)[match(base[old], .legacyRadiationNames)]
    renamed <- mapply(
      function(v, b, n) sub(paste0(b, "(_normal)?$"), paste0(n, "\\1"), v),
      vars[old],
      base[old],
      new
    )
    stop(
      "Solar radiation variables were renamed in ClimateNA v7.60 (see NEWS.md):\n  ",
      paste(vars[old], "is now", renamed, collapse = "\n  "),
      call. = FALSE
    )
  }
  invisible(vars)
}

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
