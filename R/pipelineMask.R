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
#' it. A file with nothing to blank is not rewritten. A rewritten file keeps its modification
#' time, so zips cut from it are byte-identical each time they are rebuilt.
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
  a <- .ascRead(file, mask$grid)
  cells <- sort(unique(mask$cells))
  invisible(.ascSplice(file, a, cells, rep(a$header[["nodata_value"]], length(cells))))
}

#' Write values into empty cells of a ClimateNA ASCII grid, in place
#'
#' Replaces the text of each listed cell, which must be the file's `NODATA_value`, with the given
#' value, as ClimateNA's point mode wrote it. Every other byte is left as ClimateNA wrote it, and
#' the file keeps its modification time (see [.maskAsc()]). Stops, leaving the file unchanged,
#' if a listed cell has a value (ClimateNA's own values are never overwritten) or a given value
#' is missing (`-9999` or the file's `NODATA_value`).
#'
#' @param file character. A ClimateNA `.asc` file.
#' @param fill list: `grid` (as from [.maskCells()]) for the file's tile; `cells`, the cell
#'   numbers to fill; `values`, character, one per cell.
#'
#' @return `TRUE` if the file changed, else `FALSE`, invisibly.
#'
#' @keywords internal
.fillAsc <- function(file, fill) {
  stopifnot(length(fill$cells) == length(fill$values), !anyDuplicated(fill$cells))
  if (length(fill$cells) == 0) {
    return(invisible(FALSE))
  }
  a <- .ascRead(file, fill$grid)
  nodata <- a$header[["nodata_value"]]
  missing <- fill$values %in% c("-9999", nodata) | is.na(fill$values)
  if (any(missing)) {
    stop(basename(file), ": no value for cell ", paste(fill$cells[missing], collapse = ", "))
  }
  have <- substring(a$data, a$start[fill$cells], a$end[fill$cells] - 1L)
  if (any(have != nodata)) {
    stop(
      basename(file),
      ": cell ",
      paste(fill$cells[have != nodata], collapse = ", "),
      " has a value (not NODATA)"
    )
  }
  o <- order(fill$cells)
  invisible(.ascSplice(file, a, fill$cells[o], fill$values[o]))
}

## Read a ClimateNA ASCII grid for .maskAsc() / .fillAsc(): the raw bytes, the header (named by
## lower-case keyword), the data text and each value's start and end (end exclusive) in it.
## Stops if the header's grid differs from `grid` (corners within cellsize / 1000) or the value
## count is not ncols x nrows. Values are counted in order, whatever the line breaks.
.ascRead <- function(file, grid) {
  bytes <- readBin(file, "raw", file.size(file))
  text <- rawToChar(bytes)
  nl <- gregexpr("\n", text, fixed = TRUE)[[1]]
  if (length(nl) < 6) {
    stop(basename(file), ": no ASCII grid header")
  }

  kv <- strsplit(trimws(strsplit(substr(text, 1, nl[6]), "\n", fixed = TRUE)[[1]]), "[ \t]+")
  header <- stats::setNames(vapply(kv, `[`, "", 2), tolower(vapply(kv, `[`, "", 1)))
  g <- suppressWarnings(as.numeric(header[names(grid)]))
  tol <- grid[["cellsize"]] / 1000
  if (
    anyNA(g) ||
      !is.element("nodata_value", names(header)) ||
      any(g[1:2] != grid[1:2]) ||
      any(abs(g[3:5] - grid[3:5]) > tol)
  ) {
    stop(
      basename(file),
      ": its grid (",
      paste(header, collapse = " "),
      ") differs from the DEM tile's (",
      paste(grid, collapse = " "),
      ")"
    )
  }

  data <- substr(text, nl[6] + 1L, nchar(text))
  m <- gregexpr("[^ \t\r\n]+", data)[[1]]
  n <- grid[["ncols"]] * grid[["nrows"]]
  if (length(m) != n) {
    stop(basename(file), ": ", length(m), " values, not ", n)
  }
  list(
    bytes = bytes,
    head = substr(text, 1, nl[6]),
    header = header,
    data = data,
    start = as.integer(m),
    end = as.integer(m) + attr(m, "match.length")
  )
}

## Put `values` in place of the given cells' text (cells ascending, unique) and write the file if
## that changes it, keeping its modification time. TRUE if the file changed.
.ascSplice <- function(file, a, cells, values) {
  pieces <- substring(a$data, c(1L, a$end[cells]), c(a$start[cells] - 1L, nchar(a$data)))
  data <- paste(c(rbind(pieces[-length(pieces)], values), pieces[length(pieces)]), collapse = "")
  out <- charToRaw(paste0(a$head, data))
  changed <- !identical(out, a$bytes)
  if (changed) {
    mtime <- file.mtime(file)
    writeBin(out, file)
    Sys.setFileTime(file, mtime)
  }
  changed
}

#' Blank cells in every ClimateNA ASCII grid below a folder, in place
#'
#' @param dir character. A folder, e.g. a batch's extracted period folders.
#' @param mask list from [.maskCells()] for the folder's tile.
#'
#' @return integer: how many files changed.
#'
#' @keywords internal
.maskAscDir <- function(dir, mask) {
  if (is.null(mask)) {
    stop("no mask for the tile of ", dir)
  }
  files <- list.files(dir, "[.]asc$", recursive = TRUE, full.names = TRUE)
  sum(vapply(files, .maskAsc, logical(1), mask = mask))
}
