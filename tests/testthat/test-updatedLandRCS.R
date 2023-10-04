test_that("makeLandRCS normals and projected CMI/ATA work", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  skip_if_not(interactive())

  dPath <- file.path(tempdir(), "test_makeLandRCS_1950_2010_normals")
  historicalClimatePath <- reproducible::checkPath(file.path(dPath, "climate", "historic"), create = TRUE)
  normalsClimatePath <- reproducible::checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
  normalsClimateArchive <- file.path(normalsClimatePath, "Alberta_normals.zip")

  gid <- googledrive::as_id("1UwuKTlT6zj9Qcl6AX9QnJA3rO1Itola2")
  tbl <- googledrive::drive_download(file = gid, path = normalsClimateArchive)

  archive::archive_extract(normalsClimateArchive, normalsClimatePath)

  normals <- makeLandRCS_1950_2010_normals(file.path(normalsClimatePath, "Alberta"))

  expect_s4_class(normals, "SpatRaster")
  expect_identical(names(normals), c("CMInormal", "MATnormal"))

  unlink(dPath, recursive = TRUE)

  ## makeLandRCS_projectedCMIandATA ------------------------------------------------
  dPath <- file.path(tempdir(), "test_makeLandRCS_projectedCMIandATA")

  projectedClimatePath <- reproducible::checkPath(file.path(dPath, "climate", "future", "CanESM5_ssp370"), create = TRUE)
  projAnnualClimatePath <- reproducible::checkPath(file.path(projectedClimatePath, "annual"), create = TRUE)
  projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath), "Alberta_CanESM5_ssp370_annual.zip")

  gid <- googledrive::as_id("1tK5oGja-pJrFobl-vxpD0ry7KjEWH5AR")
  tbl <- googledrive::drive_download(file = gid, path = projAnnualClimateArchive)

  archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)

  yrs <- 2030:2035
  projCMIATA <- makeLandRCS_projectedCMIandATA(
    normals[["MATnormal"]],
    file.path(projAnnualClimatePath, "Alberta"),
    years = yrs
  )

  expect_type(projCMIATA, "list")
  expect_identical(names(projCMIATA), c("projectedCMI", "projectedATA"))

  expect_s4_class(projCMIATA[["projectedCMI"]], "SpatRaster")
  expect_s4_class(projCMIATA[["projectedATA"]], "SpatRaster")

  unlink(dPath, recursive = TRUE)
})
