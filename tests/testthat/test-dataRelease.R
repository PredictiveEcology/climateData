## canClimateData caches prepClimateLayers() results; it needs an identifier that changes with
## each data release so a new release isn't served from an old cache.
test_that("climateDataRelease reads the release table", {
  skip_if_not_installed("withr")
  f <- file.path(withr::local_tempdir(), "t.sqlite")
  db <- sqlite_connect_db(f)
  .releaseCreate(db)
  .releaseWrite(
    db,
    data.frame(
      release_id = "cna-v7.70-3arcmin",
      climatena_version = "7.70",
      dem_resolution = "3arcmin",
      created = 1.7e9
    )
  )
  DBI::dbDisconnect(db)
  testthat::local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  r <- climateDataRelease()
  expect_identical(r$release_id, "cna-v7.70-3arcmin")
  expect_identical(r$climatena_version, "7.70")
  expect_identical(r$dem_resolution, "3arcmin")
})

test_that("climateDataRelease reports an index without a release table as pre-3.0.0", {
  skip_if_not_installed("withr")
  f <- file.path(withr::local_tempdir(), "t.sqlite")
  DBI::dbDisconnect(sqlite_connect_db(f))
  testthat::local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  expect_identical(climateDataRelease()$release_id, "pre-3.0.0")
})

## canClimateData calls it on every run, in many processes at once: it must read the shipped
## index where it is, read-only (no 40 MB copy), and leave the package folder as it was.
test_that("climateDataRelease reads the shipped index in place, read-only", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir()
  f <- file.path(d, "ClimateNA_tiles.sqlite")
  file.copy(ClimateNA_tiles_sqlite(), f)
  Sys.chmod(d, "0555")
  withr::defer(Sys.chmod(d, "0755"))

  realConnect <- sqlite_connect_db
  opened <- list()
  testthat::local_mocked_bindings(
    ClimateNA_tiles_sqlite = function() f,
    sqlite_connect_db = function(dbfile, readonly = FALSE) {
      opened[[length(opened) + 1]] <<- list(dbfile = dbfile, readonly = readonly)
      realConnect(dbfile, readonly = readonly)
    }
  )
  expect_identical(climateDataRelease()$release_id, "cna-v7.70-3arcmin")
  expect_identical(opened, list(list(dbfile = f, readonly = TRUE)))
  expect_identical(list.files(d), "ClimateNA_tiles.sqlite")
})

test_that(".releaseWrite keeps a single row", {
  skip_if_not_installed("withr")
  db <- sqlite_connect_db(file.path(withr::local_tempdir(), "t.sqlite"))
  withr::defer(DBI::dbDisconnect(db))
  .releaseCreate(db)
  row <- data.frame(
    release_id = "a",
    climatena_version = "7.70",
    dem_resolution = "3arcmin",
    created = 1
  )
  .releaseWrite(db, row)
  row$release_id <- "b"
  .releaseWrite(db, row)
  expect_identical(DBI::dbGetQuery(db, "SELECT release_id FROM release")$release_id, "b")
})

## A new release's zips keep the old names. In a srcdir filled from the old index, reproducible
## found 39_Y_2020.zip and its extracted files present, skipped the new download, and the old
## data (MAR, no rsds) were used (checked 2026-11-05).
test_that("prepClimateLayers downloads and mosaics in a folder named for the data release", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir("test_releaseSrcdir_")
  got <- character(0)
  testthat::local_mocked_bindings(
    getClimateURLs = function(...) character(0),
    getClimateTiles = function(tile, climateURLs, climatePath, needVars = NULL) {
      got <<- c(got, download = climatePath)
      NULL
    },
    .stopIfMissingClimateDirs = function(allDirs) invisible(allDirs),
    buildClimateMosaics = function(...) {
      got <<- c(got, mosaic = list(...)$srcdir)
      stop("stop after mosaics")
    }
  )
  vars <- list(
    MAT = list(
      vars = "historical_MAT",
      fun = quote(calcAsIs),
      .dots = list(historical_years = 2020)
    )
  )
  expect_error(
    prepClimateLayers(vars, srcdir = d, dstdir = file.path(d, "out"), tile = 39),
    "stop after mosaics"
  )
  want <- file.path(d, climateDataRelease()$release_id, "historical")
  expect_identical(unname(got), c(want, want))
})
