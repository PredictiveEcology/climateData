## The tile directories were selected with regexp "<tile>$", so tile 6 also matched directories 16,
## 26, 36, ... left in the same climate folder by an earlier, larger study area. Their extra tiles
## went into the mosaic, and where they lacked a year (no 1980s data for some tiles) the mosaics of
## different years had different extents and prepClimateLayers() stopped with
## "[rast] extents do not match" (fireSense ELF 14.3, 2026-09-12).
test_that("climateMosaicsParallel only mosaics the requested tiles", {
  skip_if_not_installed("withr")
  src <- withr::local_tempdir("historical")
  dst <- withr::local_tempdir("mosaics")
  tileRaster <- function(tile, xmin) {
    r <- terra::rast(nrows = 2, ncols = 2, xmin = xmin, xmax = xmin + 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
    terra::values(r) <- tile
    dir.create(file.path(src, tile, "Year_2000MSY"), recursive = TRUE)
    terra::writeRaster(r, file.path(src, tile, "Year_2000MSY", "CMD_sm.asc"), overwrite = TRUE)
  }
  tileRaster(6, xmin = 0)
  tileRaster(16, xmin = 10)  # a stray directory whose name ends in 6
  tileRaster(26, xmin = 20)

  out <- climateMosaicsParallel(y = "2000", climVars = "CMD_sm", tile = 6, srcdir = src, dstdir = dst)

  expect_length(out, 1)
  expect_match(basename(out), "_t_6[.]tif$")
  m <- terra::rast(out)
  expect_equal(as.vector(terra::ext(m)), c(0, 2, 0, 2), ignore_attr = TRUE)
  expect_true(all(terra::values(m) == 6))
})
