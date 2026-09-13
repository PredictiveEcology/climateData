## climateLayers() always asked for historical years 1991:2022 and projected years 2011:2100,
## so a project could not use other years without editing the list by hand.
test_that("climateLayers uses the years it is given", {
  cl <- climateLayers(c("CMD_sm", "CMD_sp"), historicalYears = 1985:2024, projectedYears = 2031:2060)

  hist <- cl[startsWith(names(cl), "historical_")]
  proj <- cl[startsWith(names(cl), "projected_")]
  expect_length(hist, 2)
  expect_length(proj, 2)
  for (h in hist) expect_identical(h$.dots, list(historical_years = 1985:2024))
  for (p in proj) expect_identical(p$.dots, list(future_years = 2031:2060))
})

test_that("climateLayers keeps its default years", {
  cl <- climateLayers("CMD_sm")
  expect_identical(cl$historical_CMDsm$.dots, list(historical_years = 1991:2022))
  expect_identical(cl$projected_CMDsm$.dots, list(future_years = 2011:2100))
})

test_that("latestHistoricalYear is the last year the tile index can supply", {
  latest <- latestHistoricalYear(tile = 1)
  expect_true(is.integer(latest))
  expect_length(latest, 1)
  expect_gt(length(getClimateURLs(type = "historical", tile = 1, years = latest, msy = "MSY")), 0)
  expect_length(getClimateURLs(type = "historical", tile = 1, years = latest + 1L, msy = "MSY"), 0)

  ## across all tiles it cannot be later than for any one of them
  expect_lte(latestHistoricalYear(), latest)
})
