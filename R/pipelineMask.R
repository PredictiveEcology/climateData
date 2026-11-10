#' Cells to blank in a tile's ClimateNA output
#'
#' The DEM tiles mark cells with no elevation as NA, and ClimateNA writes values for those cells
#' anyway, computed at the wrong elevation. They are blanked when the zips are cut, except where
#' the cell centre lies inside `boundary` (Canadian coast and lakeshore cells, which are kept).
#'
#' @param dem single-layer `SpatRaster`: the DEM tile, in longitude/latitude.
#' @param boundary `SpatVector` in the same coordinates: the area whose no-DEM cells are kept
#'   (Canada). Holes are filled first.
#'
#' @return list: `grid`, the tile's ASCII-grid header values (named `ncols`, `nrows`,
#'   `xllcorner`, `yllcorner`, `cellsize`); and `cells`, the cell numbers to blank (row by row
#'   from the top left, ascending).
#'
#' @keywords internal
.maskCells <- function(dem, boundary) {
  inside <- terra::rasterize(terra::fillHoles(boundary), dem, touches = FALSE)
  cells <- which(is.na(terra::values(dem, mat = FALSE)) & is.na(terra::values(inside, mat = FALSE)))
  e <- as.vector(terra::ext(dem))
  list(
    grid = c(
      ncols = terra::ncol(dem),
      nrows = terra::nrow(dem),
      xllcorner = e[["xmin"]],
      yllcorner = e[["ymin"]],
      cellsize = terra::xres(dem)
    ),
    cells = as.integer(cells)
  )
}
