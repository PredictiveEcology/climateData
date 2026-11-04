test_that(".periodFolder and .periodArg follow ClimateNA v7.70 naming", {
  expect_identical(.periodFolder("historical", 2025L, "MSY"), "Year_2025MSY")
  expect_identical(
    .periodFolder("future", 2025L, "MSY", "CanESM5", "245"),
    "CanESM5_ssp245@2025MSY"
  )
  expect_identical(.periodFolder("historical_normals", "1991_2020", "Y"), "Normal_1991_2020Y")
  expect_identical(
    .periodFolder("future_normals", "2011_2040", "Y", "CanESM5", "245"),
    "CanESM5_ssp245_2011-2040Y"
  )
  expect_identical(.periodArg("historical", 2025L), "Year_2025.ann")
  expect_identical(.periodArg("future", 2025L, "CanESM5", "245"), "CanESM5_ssp245@2025.gcm")
  expect_identical(.periodArg("historical_normals", "1991_2020"), "Normal_1991_2020.nrm")
  expect_identical(
    .periodArg("future_normals", "2011_2040", "CanESM5", "245"),
    "CanESM5_ssp245_2011-2040.gcm"
  )
})

test_that(".batchSpecs makes one batch per decade, with stable ids", {
  b <- .batchSpecs("historical", tiles = 39L, years = 2011:2025)
  expect_identical(vapply(b, `[[`, "", "id"), c("historical_39_2010", "historical_39_2020"))
  expect_identical(b[[2]]$periods, 2020:2025)
  expect_identical(b[[2]]$msy, "MSY")
  b26 <- .batchSpecs("historical", tiles = 39L, years = 2011:2026)
  expect_identical(b26[[1]], b[[1]]) # adding 2026 leaves the 2010s batch identical
  expect_identical(b26[[2]]$periods, 2020:2026)
})

test_that("adding a GCM or SSP leaves the existing batch specs identical", {
  old <- .batchSpecs("future", tiles = 39L, gcms = "CanESM5", ssps = "245", years = 2011:2030)
  new <- .batchSpecs(
    "future",
    tiles = 39L,
    gcms = c("CanESM5", "MIROC6"),
    ssps = c("245", "585"),
    years = 2011:2030
  )
  ids <- vapply(new, `[[`, "", "id")
  for (o in old) {
    expect_identical(new[[match(o$id, ids)]], o)
  }
})

test_that(".batchSpecs covers future and normals", {
  f <- .batchSpecs(
    "future",
    tiles = 39L,
    gcms = "CanESM5",
    ssps = c("245", "370"),
    years = 2091:2100
  )
  expect_identical(
    vapply(f, `[[`, "", "id"),
    c(
      "future_39_CanESM5_245_2090",
      "future_39_CanESM5_245_2100",
      "future_39_CanESM5_370_2090",
      "future_39_CanESM5_370_2100"
    )
  )
  n <- .batchSpecs(
    "future_normals",
    tiles = 39L,
    gcms = "CanESM5",
    ssps = "245",
    periods = c("2011_2040", "2041_2070", "2071_2100")
  )
  expect_length(n, 1)
  expect_identical(n[[1]]$id, "future_normals_39_CanESM5_245")
  expect_identical(n[[1]]$msy, "Y")
  expect_identical(n[[1]]$periods, c("2011_2040", "2041_2070", "2071_2100"))
})

test_that(".demTiles lists only tiles with a DEM file", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir()
  file.create(file.path(d, c("can_dem_1.asc", "can_dem_39.asc", "notes.txt")))
  expect_identical(.demTiles(d), c(1L, 39L))
})
