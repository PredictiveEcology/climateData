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

#' Blank cells in a ClimateNA ASCII grid, in place
#'
#' Sets the cells listed in `mask` to the file's `NODATA_value`. Only those values' text
#' changes: every other byte (other values, spacing, line endings) is left as ClimateNA wrote
#' it. A file with nothing to blank is not rewritten.
#'
#' Cells are counted in the order the values appear after the header, as GDAL reads them, not
#' by line: ClimateNA wraps some files' rows across lines (e.g. 100 values, then 77) and
#' sometimes writes a run of spaces between two values.
#'
#' @param file character. A ClimateNA `.asc` file.
#' @param mask list from [.maskCells()] for the file's tile.
#'
#' @return `TRUE` if the file changed, else `FALSE`, invisibly.
#'
#' @keywords internal
.maskAsc <- function(file, mask) {
  if (length(mask$cells) == 0) {
    return(invisible(FALSE))
  }
  bytes <- readBin(file, "raw", file.size(file))
  text <- rawToChar(bytes)
  nl <- gregexpr("\n", text, fixed = TRUE)[[1]]
  if (length(nl) < 6) {
    stop(basename(file), ": no ASCII grid header")
  }

  kv <- strsplit(trimws(strsplit(substr(text, 1, nl[6]), "\n", fixed = TRUE)[[1]]), "[ \t]+")
  header <- stats::setNames(vapply(kv, `[`, "", 2), tolower(vapply(kv, `[`, "", 1)))
  grid <- suppressWarnings(as.numeric(header[names(mask$grid)]))
  tol <- mask$grid[["cellsize"]] / 1000
  if (
    anyNA(grid) ||
      !is.element("nodata_value", names(header)) ||
      any(grid[1:2] != mask$grid[1:2]) ||
      any(abs(grid[3:5] - mask$grid[3:5]) > tol)
  ) {
    stop(
      basename(file),
      ": its grid (",
      paste(header, collapse = " "),
      ") differs from the DEM tile's (",
      paste(mask$grid, collapse = " "),
      ")"
    )
  }

  data <- substr(text, nl[6] + 1L, nchar(text))
  m <- gregexpr("[^ \t\r\n]+", data)[[1]]
  n <- mask$grid[["ncols"]] * mask$grid[["nrows"]]
  if (length(m) != n) {
    stop(basename(file), ": ", length(m), " values, not ", n)
  }
  ## splice the NODATA value in place of each masked value; the text between them is kept
  cells <- sort(unique(mask$cells))
  start <- m[cells]
  end <- start + attr(m, "match.length")[cells]
  pieces <- substring(data, c(1L, end), c(start - 1L, nchar(data)))
  data <- paste(
    c(rbind(pieces[-length(pieces)], header[["nodata_value"]]), pieces[length(pieces)]),
    collapse = ""
  )

  out <- charToRaw(paste0(substr(text, 1, nl[6]), data))
  changed <- !identical(out, bytes)
  if (changed) {
    writeBin(out, file)
  }
  invisible(changed)
}
