test_that("makeClimateDEM works", {
  skip_on_cran()
  skip_if_not(identical(tolower(.Platform$OS.type), "windows"))
  skip_if_not_installed("geodata")
  skip_if_not_installed("googledrive")
  skip_if_not_installed("withr")

  googledrive::drive_deauth()

  dPath <- withr::local_tempdir("test_makeClimateDEM_")

  canProvs <- geodata::gadm(country = "CAN", level = 1, path = dPath)
  expect_s4_class(canProvs, "SpatVector")

  studyArea <- canProvs[canProvs$NAME_1 == "Alberta", ]

  climateDEM <- makeClimateDEM(
    studyArea,
    DEMdestinationPath = dPath,
    destinationPath = dPath,
    filename2 = "Alberta"
  )
  expect_s4_class(climateDEM, "SpatRaster")

  withr::deferred_run()
})
