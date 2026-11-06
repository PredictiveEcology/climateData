## When a seasonal variable is needed, the MSY ("all") archive is downloaded; it holds every file of the
## M and Y archives too. Monthly and yearly variables were still downloaded from their own archives
## unless M, S and Y were ALL needed. Some M archives are incomplete (tile 46's future 2080s has no 2085),
## so fireSense runs needing monthly variables for cumMDC plus a seasonal CMD stopped with
## "Climate data folders are missing for tile 46 ... @2085M" (Mackenzie 4.2.1 and 4.2.2, 2026-09-24).

test_that("M and Y come from the MSY archive whenever MSY is needed", {
  expect_setequal(.consolidateClimateTypes(c("future_M", "future_S")), "future_MSY")
  expect_setequal(.consolidateClimateTypes(c("future_Y", "future_S")), "future_MSY")
  expect_setequal(.consolidateClimateTypes(c("future_M", "future_S", "future_Y")), "future_MSY")
  expect_setequal(.consolidateClimateTypes(c("historical_M", "historical_S")), "historical_MSY")
  expect_setequal(
    .consolidateClimateTypes(c("historical_M", "historical_S", "future_M")),
    c("historical_MSY", "future_M")
  )
})

test_that("without a seasonal variable, M and Y keep their own archives", {
  expect_setequal(.consolidateClimateTypes("future_M"), "future_M")
  expect_setequal(
    .consolidateClimateTypes(c("historical_M", "historical_Y")),
    c("historical_M", "historical_Y")
  )
})

## v3.0.0 data releases publish S archives, but their folders hold no PPT01-PPT12, so
## maskInvalidPPT() could not mask tiles 6 and 7's invalid cells for seasonal-only requests.
## Seasonal variables still come from the MSY archive.
test_that("seasonal-only requests use the MSY archive, which holds the monthly PPT", {
  expect_setequal(.consolidateClimateTypes("future_S"), "future_MSY")
  expect_setequal(
    .consolidateClimateTypes(c("historical_S", "future_M")),
    c("historical_MSY", "future_M")
  )
  expect_setequal(
    .consolidateClimateTypes(c("historical_N", "historical_S")),
    c("historical_N", "historical_MSY")
  )
})

test_that("prepClimateLayers downloads only the MSY archive for monthly plus seasonal variables", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir("test_consolidate_")
  requested <- character(0)
  testthat::local_mocked_bindings(
    getClimateURLs = function(type, tile, years = NULL, msy = NULL, ...) {
      requested <<- c(requested, paste0(type, "_", msy))
      character(0)
    },
    getClimateTiles = function(...) NULL,
    .stopIfMissingClimateDirs = function(allDirs) invisible(allDirs),
    buildClimateMosaics = function(...) stop("stop after downloads")
  )
  vars <- list(
    future_cumMDC = list(
      vars = c("future_PPT05", "future_Tmax05"),
      fun = quote(calcAsIs),
      .dots = list(future_years = 2083:2085)
    ),
    future_CMD_sm = list(
      vars = "future_CMD_sm",
      fun = quote(calcAsIs),
      .dots = list(future_years = 2083:2085)
    )
  )
  expect_error(
    prepClimateLayers(
      vars,
      srcdir = d,
      dstdir = file.path(d, "out"),
      tile = 46,
      gcm = "CNRM-ESM2-1",
      ssp = 370
    ),
    "stop after downloads"
  )
  expect_identical(unique(requested), "future_MSY")
})

## prepClimateLayers() used to stop with "future normals not yet implemented".
test_that("prepClimateLayers asks for future normals archives instead of stopping", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir("test_futureNormals_")
  requested <- list()
  testthat::local_mocked_bindings(
    getClimateURLs = function(type, tile, years = NULL, msy = NULL, gcm = NULL, ssp = NULL) {
      requested[[length(requested) + 1]] <<- list(type = type, gcm = gcm, ssp = as.character(ssp))
      character(0)
    },
    getClimateTiles = function(...) NULL,
    .stopIfMissingClimateDirs = function(allDirs) invisible(allDirs),
    buildClimateMosaicsNormals = function(...) stop("stop after downloads")
  )
  vars <- list(
    future_MAT_normal = list(
      vars = "future_MAT_normal",
      fun = quote(calcAsIs),
      .dots = list(future_period = "2011_2040")
    )
  )
  expect_error(
    prepClimateLayers(
      vars,
      srcdir = d,
      dstdir = file.path(d, "out"),
      tile = 39,
      gcm = "CanESM5",
      ssp = 245
    ),
    "stop after downloads"
  )
  expect_identical(requested[[1]], list(type = "future_normals", gcm = "CanESM5", ssp = "245"))
})

test_that("monthly variables load from an MSY folder", {
  skip_if_not_installed("withr")
  src <- withr::local_tempdir("future")
  dst <- withr::local_tempdir("mosaics")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r) <- 5
  msy <- file.path(src, 46, "CNRM-ESM2-1_ssp370@2085MSY")
  dir.create(msy, recursive = TRUE)
  terra::writeRaster(r, file.path(msy, "PPT05.asc"), overwrite = TRUE)

  out <- climateMosaicsParallel(
    y = "2085",
    climVars = "PPT05",
    tile = 46,
    srcdir = src,
    dstdir = dst
  )
  expect_length(out, 1)
  expect_true(all(terra::values(terra::rast(out)) == 5))
})
