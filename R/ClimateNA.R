hasClimateNA <- function() {
  ## TODO
  if (identical(tolower(.Platform$OS.type), "windows")) {
    TRUE
  } else {
    FALSE
  }
}

#' Rewrite a `.asc` raster file to use Windows (CR) line-endings
#'
#' @param f character. path to a `.asc` raster file
#'
#' @export
rewrite_asc <- function(f) {
  readLines(f, warn = FALSE) |>
    gsub("\n", "\r", x = _) |>
    writeLines(f)

  return(invisible(NULL))
}

#' Get the tile ID of a DEM raster
#'
#' @param file character. path to a DEM raster tile file.
#'
#' @export
tileID <- function(file) {
  basename(file) |>
    sub("^can_dem_", "", x = _) |>
    sub("[.]asc$", "", x = _) |>
    as.integer()
}

#' Construct target output paths for ClimateNA data
#'
#' @param dataPath character. path to top-level directory for reading/writing ClimateNA data.
#' @param tile integer. tile id (e.g., the output of `tileID()`).
#' @param type character. one of 'normals', 'historic', or 'future'.
#' @param msy character. one of 'MSY', 'M', or 'Y', corresponding to all, monthly, or yearly data.
#' @param gcm character. name of GCM available in ClimateNA.
#' @param ssp integer. one of the available SSPs in ClimateNA (e.g., `370` or `585`)
#'
#' @return character string representing the path to the target output directory
#'
#' @export
ClimateNA_path <- function(dataPath, tile = NULL, type = NULL, msy = NULL, gcm = NULL, ssp = NULL) {
  stopifnot(type %in% c("normals", "historic", "future"))

  switch(
    type,
    normals = file.path(dataPath, "historic", "normals", tile),
    historic = file.path(dataPath, "historic",
                         switch(msy, MSY = "all", M = "monthly", Y = "yearly"), tile),
    future = file.path(dataPath, "future", paste0(gcm, "_ssp", ssp),
                       switch(msy, MSY = "all", M = "monthly", Y = "yearly"), tile)
  ) |>
    normalizePath(mustWork = FALSE) |>
    (function(x) {
      if (!dir.exists(x)) dir.create(x, recursive = TRUE)
      x
    })()
}
