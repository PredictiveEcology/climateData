## filledCells(): the land cells whose values came from ClimateNA's point mode at a nearby land
## cell (or were left empty), read from the bundled index.
filledIndex <- function(rows = TRUE) {
  f <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "index.sqlite")
  db <- sqlite_connect_db(f)
  on.exit(DBI::dbDisconnect(db))
  .releaseCreate(db)
  if (rows) {
    .filledCellsCreate(db)
    .filledCellsWrite(
      db,
      data.frame(
        class = c("future", "future", "historical"),
        tile = c(63L, 63L, 52L),
        cell = c(13905L, 14081L, 100L),
        lon = c(-61.425, -61.425, -64.175),
        lat = c(66.725, 66.675, 63.625),
        elev = c("204.7481", "461.6809", "17.3577"),
        src_lon = c(-61.525, NA, -64.175),
        src_lat = c(66.725, NA, 63.475),
        km = c(4.41, NA, 16.7)
      )
    )
  }
  f
}

test_that("filledCells returns sf points with the documented columns", {
  f <- filledIndex()
  local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  x <- filledCells()
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 3L)
  expect_identical(
    setdiff(names(x), attr(x, "sf_column")),
    c("class", "tile", "cell", "elev", "filled", "src_lon", "src_lat", "km")
  )
  expect_identical(x$filled, c(TRUE, FALSE, TRUE))
  expect_identical(sf::st_crs(x)$epsg, 4326L)
  expect_equal(unname(sf::st_coordinates(x)[1, ]), c(-61.425, 66.725))
})

test_that("filledCells filters by tiles", {
  f <- filledIndex()
  local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  expect_identical(filledCells(tiles = 52)$tile, 52L)
})

test_that("filledCells keeps points inside a projected study area", {
  f <- filledIndex()
  local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  ## a 5 km square (Canada Atlas Lambert) around tile 63's first cell
  p <- sf::st_transform(sf::st_sfc(sf::st_point(c(-61.425, 66.725)), crs = 4326), 3978)
  sa <- sf::st_buffer(p, 2500, endCapStyle = "SQUARE")
  x <- filledCells(studyArea = sa)
  expect_identical(x$cell, 13905L)
  ## a SpatVector works too
  expect_identical(filledCells(studyArea = terra::vect(sa))$cell, 13905L)
})

test_that("filledCells returns no rows for an index without filled_cells", {
  f <- filledIndex(rows = FALSE)
  local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  expect_no_warning(x <- filledCells())
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 0L)
  expect_true(all(c("class", "tile", "cell", "filled") %in% names(x)))
})
