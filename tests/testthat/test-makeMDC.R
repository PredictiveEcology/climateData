test_that("makeMDC works", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  skip_if_not(interactive())

  dPath <- file.path(tempdir(), "test_makeMDC")
  historicalClimatePath <- reproducible::checkPath(file.path(dPath, "climate", "historic"), create = TRUE)
  historicalClimateArchive <- file.path(historicalClimatePath, "Alberta.zip")

  gid <- googledrive::as_id("1we9GqEVAORWLbHi3it66VnCcvLu85QIk") ## Alberta.zip
  tbl <- googledrive::drive_download(file = gid, path = historicalClimateArchive)

  archive::archive_extract(historicalClimateArchive, historicalClimatePath)

  yrs <- c(2011:2015)
  mdc <- makeMDC(tools::file_path_sans_ext(historicalClimateArchive), years = yrs, droughtMonths = 4:9)

  expect_s4_class(mdc, "SpatRaster")
  expect_identical(names(mdc), paste0("mdc", yrs))

  unlink(dPath, recursive = TRUE)
})
