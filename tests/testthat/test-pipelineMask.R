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
