## calcOMDC(): the overwintered Monthly Drought Code (FireSense, 2026-09-23). A year-level probe over six
## ELFs ranked its May-September mean with the best climate covariates for area burned, and in held-out
## spread fits it beat CMD_sm in ELF 4.3.

## one year's stack of monthly PPT01-12 and Tmax04-10 on a 1 x ncell raster
monthlyStack <- function(ppt, tmax, ncell = 2L) {
  r <- terra::rast(nrows = 1, ncols = ncell, xmin = 0, xmax = ncell, ymin = 0, ymax = 1)
  lyrs <- c(lapply(1:12, function(m) terra::setValues(r, rep_len(ppt[[m]], ncell))),
            lapply(4:10, function(m) terra::setValues(r, rep_len(tmax[[m - 3L]], ncell))))
  s <- terra::rast(lyrs)
  names(s) <- c(sprintf("PPT%02d", 1:12), sprintf("Tmax%02d", 4:10))
  s
}
omdcLayers <- paste0("historical_", c(sprintf("PPT%02d", 1:12), sprintf("Tmax%02d", 4:10)))

## An independent, deliberately plain version of the same arithmetic, one cell and one year at a time
refOMDC <- function(pptByYear, tmaxByYear) {
  Lf <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6)
  nd <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  out <- numeric(0); oct <- NA; winter <- NA
  for (y in seq_along(pptByYear)) {
    p <- pptByYear[[y]]; t <- tmaxByYear[[y]]
    dc <- if (is.na(oct)) 15 else {
      rw <- winter + p[1] + p[2] + p[3]
      max(400 * log(800 / (0.75 * 800 * exp(-oct / 400) + 0.75 * 3.94 * rw)), 15)
    }
    mids <- numeric(0)
    for (m in 4:10) {
      pe <- max(0.5 * (0.36 * (max(t[m - 3], -2.8) + 2.8) + Lf[m]), 0)
      h <- dc + 0.5 * nd[m] * pe
      e <- max(400 * log(800 / (800 * exp(-h / 400) + 3.937 * 0.83 * p[m])), 0) + 0.5 * nd[m] * pe
      if (m %in% 5:9) mids <- c(mids, (dc + e) / 2)
      dc <- e
    }
    oct <- dc; winter <- p[11] + p[12]
    out <- c(out, mean(mids))
  }
  out
}

test_that("the overwintering step matches cffdrs::overwinter_drought_code()", {
  ## cffdrs 1.9.2, overwinter_drought_code(DCf = c(50, 300, 600), rw = c(50, 100, 20), a = 0.75, b = 0.75)
  expect_equal(climateData:::.overwinterDC(c(50, 300, 600), c(50, 100, 20)),
               c(66.6296, 129.3790, 568.8140), tolerance = 1e-5)
  ## never below the standard spring start of 15
  expect_equal(climateData:::.overwinterDC(20, 1000), 15)
})

test_that("calcOMDC() carries the drought code across months and years", {
  ppt <- list(c(30, 25, 30, 35, 50, 70, 80, 70, 60, 45, 35, 30),
              c(20, 15, 20, 20, 30, 40, 50, 45, 40, 30, 25, 20),
              c(40, 35, 40, 45, 70, 90, 100, 90, 80, 55, 45, 40))
  tmax <- list(c(8, 16, 21, 24, 22, 15, 6), c(10, 19, 25, 28, 26, 18, 8), c(6, 14, 19, 21, 19, 13, 5))
  stacks <- setNames(Map(monthlyStack, ppt, tmax), paste0("historical_", 2001:2003))
  out <- calcOMDC(stacks, omdcLayers, .dots = list(historical_years = 2001:2003))
  expect_s4_class(out, "SpatRaster")
  expect_identical(names(out), paste0("oMDC_historical_", 2001:2003))
  expect_equal(unname(unlist(terra::global(out, "mean"))), refOMDC(ppt, tmax), tolerance = 1e-9)
})

test_that("a dry autumn raises next year's value; the first year has no memory", {
  wet <- c(30, 25, 30, 35, 50, 70, 80, 70, 60, 120, 120, 30)
  dry <- replace(wet, c(9, 10, 11, 12), c(5, 5, 5, 5))   # dry Sep to Dec in year 1
  tmax <- c(8, 16, 21, 24, 22, 15, 6)
  run <- function(p1) {
    s <- setNames(list(monthlyStack(p1, tmax), monthlyStack(wet, tmax)), paste0("historical_", 2001:2002))
    unname(unlist(terra::global(calcOMDC(s, omdcLayers, list(historical_years = 2001:2002)), "mean")))
  }
  wetRun <- run(wet); dryRun <- run(dry)
  expect_gt(dryRun[2], wetRun[2])           # carried over into 2002
  ## calcMDC() restarts every month from 0, so the same autumn leaves no trace there
})

test_that("calcOMDC() refuses gaps between years and missing months", {
  s <- setNames(list(monthlyStack(rep(30, 12), rep(15, 7)), monthlyStack(rep(30, 12), rep(15, 7))),
                paste0("historical_", c(2001, 2003)))
  expect_error(calcOMDC(s, omdcLayers, list(historical_years = c(2001, 2003))), "consecutive")
  s2 <- setNames(list(monthlyStack(rep(30, 12), rep(15, 7))[[-1]]), "historical_2001")
  expect_error(calcOMDC(s2, omdcLayers[-1], list(historical_years = 2001)), "PPT01")
})

test_that("climateLayers('oMDC') requests the monthly inputs and spin-up years", {
  cl <- climateLayers("oMDC", historicalYears = 1985:2024, projectedYears = 2025:2044, spinupYears = 5)
  h <- cl[["historical_oMDC"]]
  expect_identical(h$fun, quote(calcOMDC))
  expect_setequal(h$vars, paste0("historical_", c(sprintf("PPT%02d", 1:12), sprintf("Tmax%02d", 4:10))))
  expect_identical(h$.dots$historical_years, 1980:2024)
  p <- cl[["projected_oMDC"]]
  expect_identical(p$.dots$future_years, 2020:2044)
  ## spin-up cannot go before the first year that exists
  expect_identical(climateLayers("oMDC", historical = FALSE, projectedYears = 2011:2020)[[1]]$.dots$future_years,
                   2011:2020)
  ## other variables are unchanged
  expect_identical(climateLayers("CMD_sm", historicalYears = 1991:2000)[["historical_CMDsm"]]$.dots$historical_years,
                   1991:2000)
})
