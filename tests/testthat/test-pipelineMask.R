## The DEM tiles mark cells with no elevation as NA, and ClimateNA computes those cells anyway at
## the wrong elevation. Outside Canada they are blanked when zips are cut; inside Canada (coast and
## lakeshore cells) they are kept. Holes in the boundary are filled first, as for the tile grid.
test_that(".maskCells blanks no-DEM cells outside the boundary only", {
  dem <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(dem) <- 100
  dem[c(1, 4, 13, 16)] <- NA
  ## left half, with a hole over cell 1 (x 0-1, y 3-4)
  outer <- cbind(c(0, 2, 2, 0, 0), c(0, 0, 4, 4, 0))
  hole <- cbind(c(0.2, 0.8, 0.8, 0.2, 0.2), c(3.2, 3.2, 3.8, 3.8, 3.2))
  boundary <- terra::vect(
    rbind(cbind(1, 1, outer, 0), cbind(1, 1, hole, 1)),
    type = "polygons",
    crs = "EPSG:4326"
  )

  m <- .maskCells(dem, boundary)
  expect_identical(m$cells, c(4L, 16L))
  expect_equal(m$grid, c(ncols = 4, nrows = 4, xllcorner = 0, yllcorner = 0, cellsize = 1))
})

## An ASCII grid as ClimateNA writes it: header lines end in CRLF; each data row is the values
## separated by single spaces, then a space and CRLF.
writeClimatenaAsc <- function(file, tokens, ncols, nrows, xll = "-123.350000000000", eol = "\r\n") {
  header <- sprintf(
    "%-13s%s",
    c("ncols", "nrows", "xllcorner", "yllcorner", "cellsize", "NODATA_value"),
    c(ncols, nrows, xll, "45.800000000000", "0.050000000000", "-9999")
  )
  rows <- vapply(
    split(tokens, rep(seq_len(nrows), each = ncols)),
    \(x) paste0(paste(x, collapse = " "), " "),
    ""
  )
  writeBin(charToRaw(paste0(c(header, rows), eol, collapse = "")), file)
  file
}
gridOf <- function(ncols, nrows, xll = -123.35) {
  c(ncols = ncols, nrows = nrows, xllcorner = xll, yllcorner = 45.8, cellsize = 0.05)
}
bytesOf <- function(f) readBin(f, "raw", file.size(f))

test_that(".maskAsc sets masked cells to NODATA and changes no other byte", {
  tokens <- c("8", "-0.4", "12.3", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  masked <- replace(tokens, c(2, 7), "-9999")
  for (eol in c("\r\n", "\n")) {
    f <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), tokens, 4, 3, eol = eol)
    want <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), masked, 4, 3, eol = eol)
    expect_true(.maskAsc(f, list(grid = gridOf(4, 3), cells = c(2L, 7L))))
    expect_identical(bytesOf(f), bytesOf(want))
  }
})

## ClimateNA sometimes writes a run of spaces between two values (seen in tile 6's MAT.asc)
test_that(".maskAsc finds values by position when spaces between them vary", {
  f <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), as.character(1:12), 4, 3)
  txt <- sub("2 3 4 ", "2               3 4 ", rawToChar(bytesOf(f)), fixed = TRUE)
  writeBin(charToRaw(txt), f)
  expect_true(.maskAsc(f, list(grid = gridOf(4, 3), cells = c(3L, 4L))))
  expect_identical(
    rawToChar(bytesOf(f)),
    sub("2               3 4 ", "2               -9999 -9999 ", txt, fixed = TRUE)
  )
})

## ...and wraps some files' rows across lines (CMD01.asc: 100 values, then 77, per 177-cell row);
## readers take the values in order, whatever the line breaks
test_that(".maskAsc counts values in order across wrapped lines", {
  f <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), as.character(1:12), 3, 4)
  wrapped <- withr::local_tempfile(fileext = ".asc")
  ## 4 x 3 grid written 3 values to a line: the second row starts mid-line
  txt <- sub("1 2 3 \r\n4 5 6 \r\n", "1 2 \r\n3 4 5 \r\n6 ", rawToChar(bytesOf(f)), fixed = TRUE)
  txt <- sub("ncols        3", "ncols        4", txt, fixed = TRUE)
  txt <- sub("nrows        4", "nrows        3", txt, fixed = TRUE)
  writeBin(charToRaw(txt), wrapped)
  expect_true(.maskAsc(wrapped, list(grid = gridOf(4, 3), cells = c(4L, 5L))))
  expect_identical(
    rawToChar(bytesOf(wrapped)),
    sub("3 4 5 \r\n", "3 -9999 -9999 \r\n", txt, fixed = TRUE)
  )
})

test_that(".maskAsc leaves the file untouched when there is nothing to mask", {
  f <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), as.character(1:12), 4, 3)
  before <- bytesOf(f)
  Sys.setFileTime(f, as.POSIXct("2020-01-01", tz = "UTC"))
  expect_false(.maskAsc(f, list(grid = gridOf(4, 3), cells = integer(0))))
  expect_identical(bytesOf(f), before)
  expect_equal(as.numeric(file.mtime(f)), as.numeric(as.POSIXct("2020-01-01", tz = "UTC")))
})

## a zip is byte-identical only when its files' times are (pipeline_write_zip()), so a masked file
## keeps its time: recutting a tile's zips then gives the same MD5s and nothing to upload
test_that(".maskAsc keeps the file's modification time", {
  f <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), as.character(1:12), 4, 3)
  t0 <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
  Sys.setFileTime(f, t0)
  expect_true(.maskAsc(f, list(grid = gridOf(4, 3), cells = 2L)))
  expect_equal(as.numeric(file.mtime(f)), as.numeric(t0))
})

test_that(".maskAsc stops when the file's grid differs from the DEM's", {
  f <- writeClimatenaAsc(withr::local_tempfile(fileext = ".asc"), as.character(1:12), 4, 3)
  expect_error(.maskAsc(f, list(grid = gridOf(5, 3), cells = 1L)), basename(f), fixed = TRUE)
  expect_error(
    .maskAsc(f, list(grid = gridOf(4, 3, xll = -123.3), cells = 1L)),
    basename(f),
    fixed = TRUE
  )
})

test_that(".maskAsc compares corner coordinates within a tolerance", {
  ## 1 arcmin tiles: the header has 12 decimals, the DEM's extent more
  f <- writeClimatenaAsc(
    withr::local_tempfile(fileext = ".asc"),
    as.character(1:12),
    4,
    3,
    xll = "-123.333333333333"
  )
  expect_true(.maskAsc(f, list(grid = gridOf(4, 3, xll = -123 - 1 / 3), cells = 1L)))
})

## zip_batch() masks every grid of a batch's extracted periods before cutting the zips
test_that(".maskAscDir masks every .asc below a folder and nothing else", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "Year_2020MSY"))
  dir.create(file.path(d, "Year_2021MSY"))
  a <- writeClimatenaAsc(file.path(d, "Year_2020MSY", "MAT.asc"), as.character(1:12), 4, 3)
  b <- writeClimatenaAsc(file.path(d, "Year_2021MSY", "MAT.asc"), as.character(1:12), 4, 3)
  other <- file.path(d, "Year_2021MSY", "notes.txt")
  writeLines("1 2 3", other)
  want <- writeClimatenaAsc(
    withr::local_tempfile(fileext = ".asc"),
    replace(as.character(1:12), 2, "-9999"),
    4,
    3
  )

  expect_identical(.maskAscDir(d, list(grid = gridOf(4, 3), cells = 2L)), 2L)
  expect_identical(bytesOf(a), bytesOf(want))
  expect_identical(bytesOf(b), bytesOf(want))
  expect_identical(readLines(other), "1 2 3")
})

test_that(".maskAscDir stops when the tile has no mask", {
  expect_error(.maskAscDir(withr::local_tempdir(), NULL), "no mask")
})
