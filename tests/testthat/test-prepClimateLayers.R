test_that("prepClimateLayers works for multiple variable types", {
  skip_on_cran()
  skip_on_ci() ## needs to authorize googldrive package for downloads
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  dPath <- file.path(tempdir(), "test_prepClimateLayers")
  climateType <- "historical"
  climatePath <- file.path(dPath, "climate") |> reproducible::checkPath(create = TRUE)
  climatePathOut <- file.path(climatePath, "outputs") |> reproducible::checkPath(create = TRUE)
  tileIDs <- c(16, 17, 26, 27)
  historical_prd <- c("1951_1980", "1981_2010")
  historical_yrs <- c(2011:2015)
  future_yrs <- c(2021:2025)
  GCM <- "CanESM5"
  SSP <- 370

  ## ATA mixes seasonal and normal variables; uses custom fun
  ## CMI uses yearly variable; no fun (as is)
  ## FFP uses yearly  variable; no fun (as is); similar name to bFFP and eFFP
  ## MDC uses monthly vars; uses custom fun
  ## CMD uses seasonal variable; no fun (as is)
  climateVariables <- list(
    historical_ATA = list(
      vars = c("historical_MAT", "historical_MAT_normal"),
      fun = quote(calcATA),
      .dots = list(historical_period = historical_prd, historical_years = historical_yrs)
    ),
    future_ATA = list(
      vars = c("future_MAT", "historical_MAT_normal"),
      fun = quote(calcATA),
      .dots = list(historical_period = historical_prd, future_years = future_yrs)
    ),
    historical_CMI = list(
      vars = "historical_CMI",
      fun = quote(calcAsIs),
      .dots = list(historical_years = historical_yrs)
    ),
    historical_CMI_normal = list(
      vars = "historical_CMI_normal",
      fun = quote(calcCMInormal),
      .dots = list(historical_period = historical_prd, historical_years = historical_yrs)
    ),
    future_FFP = list(
      vars = "future_FFP", ## ensure FFP only; not bFFP nor eFFP
      fun = quote(calcAsIs),
      .dots = list(future_years = future_yrs)
    ),
    historical_MDC = list(
      vars = c(sprintf("historical_PPT%02d", 4:9), sprintf("historical_Tmax%02d", 4:9)),
      fun = quote(calcMDC),
      .dots = list(historical_years = historical_yrs)
    ),
    future_CMD_sm = list(
      vars = "future_CMD_sm",
      fun = quote(calcAsIs),
      .dots = list(future_years = future_yrs)
    )
  )

  climateRasters <- prepClimateLayers(
    climateVarsList = climateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    tile = tileIDs,
    historical_years = historical_yrs,
    future_years = future_yrs,
    historical_period = historical_prd,
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

  ## spurious warning:
  ## attribute variables are assumed to be spatially constant throughout all geometries
  climateRastersStudyArea <- suppressWarnings({
    prepClimateLayers(
      climateVarsList = climateVariables,
      srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
      dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
      # tile = tileIDs, ## when passing `studyArea`/`rasterToMatch` then `tile` isn't needed
      historical_years = historical_yrs,
      future_years = future_yrs,
      historical_period = historical_prd,
      future_period = NULL,
      gcm = GCM,
      ssp = SSP,
      cl = NULL,
      studyArea = studyArea,
      studyAreaName = "test_study_area",
      rasterToMatch = rasterToMatch
    )
  })

  lapply(climateRastersStudyArea, function(x) {
    expect_s4_class(x, "SpatRaster")
  })

  unlink(dPath, recursive = TRUE)
})

test_that("prepClimateLayers properly handles unordered tileIDs", {
  skip_on_cran()
  skip_on_ci() ## needs to authorize googldrive package for downloads
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  dPath <- file.path(tempdir(), "test_prepClimateLayers")
  climateType <- "historical"
  climatePath <- file.path(dPath, "climate") |> reproducible::checkPath(create = TRUE)
  climatePathOut <- file.path(climatePath, "outputs") |> reproducible::checkPath(create = TRUE)
  tileIDs <- c(3, 2, 12, 11)
  historical_yrs <- c(2005:2015) ## cross-decade, therefore multiple climate URLs

  climateVariables <- list(
    historical_DD5_sp = list(
      vars = "historical_DD5_sp",
      fun = quote(calcAsIs),
      .dots = list(historical_years = historical_yrs)
    ),
    historical_Tmin_wt = list(
      vars = "historical_Tmin_wt",
      fun = quote(calcAsIs),
      .dots = list(historical_years = historical_yrs)
    )
  )

  climateRasters <- prepClimateLayers(
    climateVarsList = climateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    tile = tileIDs,
    historical_years = historical_yrs,
    future_years = NULL,
    historical_period = NULL,
    future_period = NULL,
    gcm = NULL,
    ssp = NULL,
    cl = NULL,
    studyArea = NULL,
    studyAreaName = NULL,
    rasterToMatch = NULL
  )

  lapply(climateRasters, function(x) {
    expect_s4_class(x, "SpatRaster")
  })

  unlink(dPath, recursive = TRUE)
})
