## ClimateNA v7.60 renamed RAD to rsds. Requests for the old names should stop at once with
## the new name, not after downloading the archives with an unclear error.
test_that("pre-v7.60 radiation names stop with the new name", {
  expect_silent(.stopIfLegacyRadiation(c(
    "historical_CMD",
    "future_rsds07",
    "historical_rsds_normal"
  )))
  expect_snapshot(
    .stopIfLegacyRadiation(c(
      "historical_MAR",
      "future_Rad07",
      "historical_Rad_sm",
      "historical_MAR_normal"
    )),
    error = TRUE
  )
})

test_that("prepClimateLayers stops on pre-v7.60 radiation names before downloading", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir()
  expect_snapshot(
    prepClimateLayers(
      climateVarsList = list(
        historical_MAR = list(
          vars = "historical_MAR",
          fun = quote(calcAsIs),
          .dots = list(historical_years = 2000)
        )
      ),
      srcdir = d,
      dstdir = d,
      tile = 39
    ),
    error = TRUE
  )
})
