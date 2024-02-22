test_that("prepClimateLayers works for multiple variable types", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  dPath <- file.path(tempdir(), "test_prepClimateLayers")
  climateType <- "historic"
  climatePath <- file.path(dPath, "climate") |> reproducible::checkPath(create = TRUE)
  climatePathOut <- file.path(climatePath, "outputs") |> reproducible::checkPath(create = TRUE)
  tileIDs <- c(16, 17, 26, 27)
  historic_prd <- c("1951_1980", "1981_2010")
  historic_yrs <- c(2011:2015)
  future_yrs <- c(2021:2025)
  GCM <- "CanESM5"
  SSP <- 370

  ## ATA mixes seasonal and normal variables; uses custom fun
  ## CMI uses yearly variable; no fun (as is)
  ## FFP uses yearly  variable; no fun (as is); similar name to bFFP and eFFP
  ## MDC uses monthly vars; uses custom fun
  climateVariables <- list(
    historic_ATA = list(
      vars = c("historic_MAT", "historic_MAT_normal"),
      fun = quote(calcATA),
      .dots = list(historic_period = historic_prd, historic_years = historic_yrs)
    ),
    future_ATA = list(
      vars = c("future_MAT", "historic_MAT_normal"),
      fun = quote(calcATA),
      .dots = list(historic_period = historic_prd, future_years = future_yrs)
    ),
    historic_CMI = list(
      vars = "historic_CMI",
      fun = quote(calcAsIs),
      .dots = list(historic_years = historic_yrs)
    ),
    historic_CMI_normal = list(
      vars = "historic_CMI_normal",
      fun = quote(calcCMInormal),
      .dots = list(historic_period = historic_prd, historic_years = historic_yrs)
    ),
    future_FFP = list(
      vars = "future_FFP", ## ensure FFP only; not bFFP nor eFFP
      fun = quote(calcAsIs),
      .dots = list(future_years = future_yrs)
    ),
    historic_MDC = list(
      vars = c(sprintf("historic_PPT%02d", 4:9), sprintf("historic_Tmax%02d", 4:9)),
      fun = quote(calcMDC),
      .dots = list(historic_years = historic_yrs)
    )
  )

  climateRasters <- prepClimateLayers(
    climateVarsList = climateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    tile = tileIDs,
    historic_years = historic_yrs,
    future_years = future_yrs,
    historic_period = historic_prd,
    future_period = NULL,
    gcm = GCM,
    ssp = SSP,
    cl = NULL,
    studyArea = NULL,
    studyAreaName = NULL,
    rasterToMatch = NULL
  )

  lapply(climateRasters, function(x) {
    expect_s4_class(x, "SpatRaster")
  })

  ## now test with studyArea and rasterTaMatch
  skip_if_not_installed("SpaDES.tools")

  studyArea <- SpaDES.tools::randomStudyArea(size = 1e10)
  rasterToMatch <- terra::rast(studyArea, resolution = 250) |>
    terra::rasterize(studyArea, y = _)

  climateRastersStudyArea <- prepClimateLayers(
    climateVarsList = climateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    # tile = tileIDs, ## when passing `studyArea`/`rasterToMatch` then `tile` isn't needed
    historic_years = historic_yrs,
    future_years = future_yrs,
    historic_period = historic_prd,
    future_period = NULL,
    gcm = GCM,
    ssp = SSP,
    cl = NULL,
    studyArea = studyArea,
    studyAreaName = "test_study_area",
    rasterToMatch = rasterToMatch
  )

  lapply(climateRastersStudyArea, function(x) {
    expect_s4_class(x, "SpatRaster")
  })

  unlink(dPath, recursive = TRUE)
})
