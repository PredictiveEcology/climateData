test_that("missing climate folders are named in the error", {
  skip_if_not_installed("withr")

  d <- withr::local_tempdir("test_missingClimateDirs_")
  present <- file.path(d, "39", "CNRM-ESM2-1_ssp370@2012MSY")
  absent <- file.path(d, "39", c("CNRM-ESM2-1_ssp370@2013MSY", "CNRM-ESM2-1_ssp370@2014MSY"))
  dir.create(present, recursive = TRUE)

  expect_silent(.stopIfMissingClimateDirs(present))

  err <- tryCatch(.stopIfMissingClimateDirs(c(present, absent)), error = conditionMessage)
  expect_match(err, "tile 39", fixed = TRUE)
  expect_match(err, "@2013MSY", fixed = TRUE)
  expect_match(err, "@2014MSY", fixed = TRUE)
  expect_false(grepl("@2012MSY", err, fixed = TRUE))
})
