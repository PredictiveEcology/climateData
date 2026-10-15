test_that("a start inside the span of the climate data is accepted", {
  expect_silent(checkCalendarYears(2020, 2120, module = "m"))
  expect_silent(checkCalendarYears(1901, 1950, module = "m"))
  expect_silent(checkCalendarYears(2100, 2100, module = "m"))
  expect_silent(checkCalendarYears(1985.5, 2020, module = "m"))
})

test_that("the span is the one available() reports", {
  yrs <- range(unlist(lapply(c("historical", "future"), function(t) available(t)[["years"]])))
  expect_error(checkCalendarYears(yrs[1] - 1, 2000, module = "m"), as.character(yrs[1]))
  expect_error(checkCalendarYears(yrs[2] + 1, 2200, module = "m"), as.character(yrs[2]))
})

test_that("model-year times fail with the module, both times and an example", {
  expect_error(checkCalendarYears(0, 10, module = "canClimateData"),
               "canClimateData: times are model years 0-10.*calendar years.*times = list\\(start = 2020, end = 2120\\)")
})

test_that("a hint is appended to the message", {
  expect_error(checkCalendarYears(0, 10, module = "m", hint = "Do this instead."), "Do this instead\\.")
})

test_that("`years` replaces the span of available()", {
  expect_silent(checkCalendarYears(1995, 2050, module = "m", years = 1991:2100))
  expect_error(checkCalendarYears(1950, 2050, module = "m", years = 1991:2100), "1991-2100")
  expect_error(checkCalendarYears(2101, 2200, module = "m", years = 1991:2100), "1991-2100")
})
