test_that(".writeClimateStack writes band-interleaved tiles with identical values", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 300, ncols = 400, nlyrs = 6, xmin = 0, xmax = 400, ymin = 0, ymax = 300)
  set.seed(1)
  terra::values(r) <- round(stats::rnorm(terra::ncell(r) * 6, 100, 30), 1)
  names(r) <- paste0("year", 2001:2006)

  oldFile <- withr::local_tempfile(fileext = ".tif")
  newFile <- withr::local_tempfile(fileext = ".tif")
  ## how the stacks were written before: terra default, pixel-interleaved 1-row strips
  terra::writeRaster(r, oldFile, gdal = "INTERLEAVE=PIXEL", overwrite = TRUE)
  expect_match(
    sf::gdal_utils("info", oldFile, quiet = TRUE),
    "INTERLEAVE=PIXEL"
  )

  out <- climateData:::.writeClimateStack(r, newFile)
  info <- sf::gdal_utils("info", newFile, quiet = TRUE)
  expect_match(info, "Block=256x256")
  expect_match(info, "Block=256x256")
  expect_match(info, "COMPRESSION=LZW")

  expect_identical(names(terra::rast(newFile)), names(r))
  expect_equal(terra::values(terra::rast(newFile)), terra::values(terra::rast(oldFile)),
               tolerance = 0)
  expect_equal(terra::values(out), terra::values(terra::rast(oldFile)), tolerance = 0)
  ## one layer on its own is the same too
  expect_equal(terra::values(terra::rast(newFile, lyrs = "year2004")),
               terra::values(terra::rast(oldFile, lyrs = "year2004")), tolerance = 0)

  ## a single layer is not given tiling
  oneFile <- withr::local_tempfile(fileext = ".tif")
  climateData:::.writeClimateStack(r[[1]], oneFile)
  expect_no_match(sf::gdal_utils("info", oneFile, quiet = TRUE), "Block=256x256")

  ## overwriting an existing file works
  climateData:::.writeClimateStack(r, newFile)
  expect_equal(terra::nlyr(terra::rast(newFile)), 6L)
})

test_that(".postProcessAndWriteClimate gives the same layers as postProcessTo(writeTo =)", {
  skip_if_not_installed("terra")
  skip_if_not_installed("reproducible")
  r <- terra::rast(nrows = 120, ncols = 160, nlyrs = 4, xmin = 0, xmax = 160, ymin = 0, ymax = 120,
                   crs = "EPSG:3857")
  set.seed(2)
  terra::values(r) <- round(stats::rnorm(terra::ncell(r) * 4, 50, 10), 2)
  names(r) <- paste0("year", 2001:2004)
  to <- terra::rast(terra::ext(20, 120, 10, 90), res = 2, crs = "EPSG:3857")
  mask <- terra::as.polygons(terra::ext(30, 100, 20, 80), crs = "EPSG:3857")

  oldFile <- withr::local_tempfile(fileext = ".tif")
  newFile <- withr::local_tempfile(fileext = ".tif")
  old <- reproducible::postProcessTo(r, to = to, maskTo = mask, writeTo = oldFile,
                                     useCache = FALSE, overwrite = TRUE)
  new <- climateData:::.postProcessAndWriteClimate(r, to = to, maskTo = mask, writeTo = newFile)
  expect_equal(terra::values(new), terra::values(old), tolerance = 0)
  expect_identical(names(new), names(old))
  expect_equal(terra::values(terra::rast(newFile)), terra::values(terra::rast(oldFile)),
               tolerance = 0)
  expect_match(sf::gdal_utils("info", newFile, quiet = TRUE), "Block=256x256")
})
