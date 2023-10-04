test_that("makeLandRCS() works", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  skip_if_not(interactive())

  dPath <- file.path(tempdir(), "test_makeLandRCS")

  ## normals
  historicalClimatePath <- reproducible::checkPath(file.path(dPath, "climate", "historic"), create = TRUE)
  normalsClimatePath <- reproducible::checkPath(file.path(historicalClimatePath, "normals"), create = TRUE)
  normalsClimateArchive <- file.path(normalsClimatePath, "Alberta_normals.zip")

  gid <- googledrive::as_id("1UwuKTlT6zj9Qcl6AX9QnJA3rO1Itola2")
  tbl <- googledrive::drive_download(file = gid, path = normalsClimateArchive)

  archive::archive_extract(normalsClimateArchive, normalsClimatePath)

  ## future climate
  projectedClimatePath <- reproducible::checkPath(file.path(dPath, "climate", "future", "CanESM5_ssp370"), create = TRUE)
  projAnnualClimatePath <- reproducible::checkPath(file.path(projectedClimatePath, "annual"), create = TRUE)
  projAnnualClimateArchive <- file.path(dirname(projAnnualClimatePath), "Alberta_CanESM5_ssp370_annual.zip")

  gid <- googledrive::as_id("1tK5oGja-pJrFobl-vxpD0ry7KjEWH5AR")
  tbl <- googledrive::drive_download(file = gid, path = projAnnualClimateArchive)

  archive::archive_extract(projAnnualClimateArchive, projAnnualClimatePath)

  ## test makeLandRCS()
  dem <- terra::rast(system.file("extdata/Alberta.asc", package = "climateData"))
  yrs <- 2030:2035
  projCMIATA <- makeLandRCS(
    pathToNormalRasters = file.path(normalsClimatePath, "Alberta"),
    pathToFutureRasters = file.path(projAnnualClimatePath, "Alberta"),
    rasterPrefix = "AB",
    outputDir = dPath,
    origTemplate = dem,
    years = yrs,
    writeCMINormal = FALSE
  )

  fATA <- file.path(dPath, paste0(
    "AB", "_ATA", yrs[1], "-",
    utils::tail(yrs, 1), ".grd"
  ))

  fCMI <- file.path(dPath, paste0(
    "AB", "_CMI", yrs[1], "-",
    utils::tail(yrs, 1), ".grd"
  ))

  expect_true(file.exists(fATA))
  expect_true(file.exists(fCMI))

  unlink(dPath, recursive = TRUE)
})
