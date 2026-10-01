## needs reproducible (>= 3.2.1.9057), in which `writeTo()` honours `gdal` (DESCRIPTION Imports)
test_that("postProcessTo(writeTo, gdal = .climateStackGdalOptions) writes band-interleaved tiles with identical values", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not_installed("withr")
  r <- terra::rast(nrows = 300, ncols = 400, nlyrs = 6, xmin = 0, xmax = 400, ymin = 0, ymax = 300,
                   crs = "EPSG:3857")
  set.seed(1)
  terra::values(r) <- round(stats::rnorm(terra::ncell(r) * 6, 100, 30), 1)
  names(r) <- paste0("year", 2001:2006)
  to <- terra::rast(terra::ext(20, 380, 10, 280), resolution = 1, crs = "EPSG:3857")
  mask <- terra::as.polygons(terra::ext(30, 300, 20, 250), crs = "EPSG:3857")

  oldFile <- withr::local_tempfile(fileext = ".tif")
  newFile <- withr::local_tempfile(fileext = ".tif")
  ## how the stacks were written before: no creation options
  old <- reproducible::postProcessTo(r, to = to, maskTo = mask, writeTo = oldFile,
                                     useCache = FALSE, overwrite = TRUE)
  new <- reproducible::postProcessTo(r, to = to, maskTo = mask, writeTo = newFile,
                                     gdal = climateData:::.climateStackGdalOptions,
                                     useCache = FALSE, overwrite = TRUE)
  expect_match(sf::gdal_utils("info", oldFile, quiet = TRUE), "INTERLEAVE=PIXEL")
  info <- sf::gdal_utils("info", newFile, quiet = TRUE)
  expect_match(info, "INTERLEAVE=BAND")
  expect_match(info, "Block=256x256")
  expect_match(info, "COMPRESSION=LZW")

  expect_identical(names(terra::rast(newFile)), names(terra::rast(oldFile)))
  expect_equal(terra::values(terra::rast(newFile)), terra::values(terra::rast(oldFile)),
               tolerance = 0)
  expect_equal(terra::values(new), terra::values(old), tolerance = 0)
  ## one layer on its own is the same too
  expect_equal(terra::values(terra::rast(newFile, lyrs = "year2004")),
               terra::values(terra::rast(oldFile, lyrs = "year2004")), tolerance = 0)
})
