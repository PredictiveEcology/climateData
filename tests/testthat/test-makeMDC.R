test_that("makeMDC works", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  skip_if_not(interactive())

  dPath <- file.path(tempdir(), "test_makeMDC")
  climateType <- "historic"
  climatePath <- reproducible::checkPath(file.path(dPath, "climate", climateType), create = TRUE)
  tileIDs <- c(16, 17, 26, 27)
  yrs <- c(2011:2015)

  tileURLs <- getClimateURLs(type = climateType, tiles = tileIDs, years = yrs, msy = "M")

  getClimateTiles(tileIDs, tileURLs, climatePath)
  allDirs <- file.path(climatePath, rep(tileIDs, length(yrs)), paste0("Year_", yrs, "M")) |> sort()
  expect_true(all(dir.exists(allDirs)))

  mdc <- makeMDC(srcdir = climatePath, dstdir = dPath,
                 tiles = tileIDs, years = yrs, droughtMonths = 4:9, cl = NULL)

  expect_s4_class(mdc, "SpatRaster")
  expect_identical(names(mdc), paste0("mdc", yrs))

  unlink(dPath, recursive = TRUE)
})
