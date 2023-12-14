hasClimateNA <- function() {
  if (identical(tolower(.Platform$OS.type), "windows") &&
      dir.exists()) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
rewrite_asc <- function(f){
  readLines(f, warn = FALSE) |>
    gsub("\n", "\r", x = _) |>
    writeLines(f)

  return(invisible(NULL))
}

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
#'
#' @return character string representing the path to the target output directory
#'
#' @export
ClimateNA_path <- function(dataPath, tile = NULL, type = NULL, msy = NULL, gcm = NULL, ssp = NULL) {
  stopifnot(type %in% c("normals", "historic", "future"))

  switch(
    type,
    normals = file.path(dataPath, "tiled", "historic", "normals", tile),
    historic = file.path(dataPath, "tiled", "historic",
                         switch(msy, MSY = "all", M = "monthly", Y = "yearly"), tile),
    future = file.path(dataPath, "tiled", "future", paste0(gcm, "_ssp", ssp),
                       switch(msy, MSY = "all", M = "monthly", Y = "yearly"), tile)
  ) |>
    normalizePath(mustWork = FALSE) |>
    (function(x) {
      if (!dir.exists(x)) dir.create(x, recursive = TRUE)
      x
    })()
}
