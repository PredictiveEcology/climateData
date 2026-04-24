test_that("prepClimateLayers works for multiple variable types", {
  skip_on_cran()
  skip_on_ci() ## needs to authorize googldrive package for downloads
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")
  skip_if_not_installed("withr")

  dPath <- withr::local_tempdir("test_prepClimateLayers_")
  climateType <- "historical"
  climatePath <- file.path(dPath, "climate") |> reproducible::checkPath(create = TRUE)
  climatePathOut <- file.path(climatePath, "outputs") |> reproducible::checkPath(create = TRUE)
  tileIDs <- c(16, 17, 26, 27)
  historical_prd <- c("1951_1980", "1981_2010")
  historical_yrs <- c(2011:2015)
  future_yrs <- c(2021:2025)
  future_prd <- c("2011_2040", "2041_2070")
  GCM <- "CanESM5"
  SSP <- 370

  ## ATA mixes seasonal and normal variables; uses custom fun
  ## CMI uses yearly variable; no fun (as is)
  ## FFP uses yearly  variable; no fun (as is); similar name to bFFP and eFFP
  ## MDC uses monthly vars; uses custom fun
  ## CMD uses seasonal variable; no fun (as is)
  climateVariables <- list(
    historical_CMI = list(
      vars = "historical_CMI",
      fun = quote(calcAsIs),
      .dots = list(historical_years = 2008)
    ),
    historical_FFP_normal = list(
      vars = "historical_MAT_normal", ## ensure MAT only; not bMAT nor eMAT
      fun = quote(calcAsIs),
      .dots = list(historical_period = historical_prd)
    ),
    historical_CMI_normal = list(
      vars = "historical_CMI_normal", ## ensure MAT only; not bFFP nor eFFP
      fun = quote(calcAsIs),
      .dots = list(historical_period = historical_prd)
    ),
    historical_MAP = list(
      vars = "historical_MAP",
      fun = quote(calcAsIs),
      .dots = list(historical_years = historical_yrs)
    ),
    # future period not supported
    # future_CMD_sm_normal = list(
    #   vars = "future_CMD_sm_normal",
    #   fun = quote(calcAsIs),
    #   .dots = list(future_period = future_prd),
    future_FFP = list(
      vars = "future_FFP", ## ensure FFP only; not bFFP nor eFFP
      fun = quote(calcAsIs),
      .dots = list(future_years = future_yrs)
    ),
    historical_MDC = list(
      vars = c(sprintf("historical_PPT%02d", 4:9), sprintf("historical_Tmax%02d", 4:9)),
      fun = quote(calcMDC),
      .dots = list(historical_years = historical_yrs)
    )

  )

  ## test with all parallel backends
  climateRasters <- list()
  for (backend in .parallelBackends) {
    withr::with_options(list(climateData.parallel.backend = backend), {
      climateRasters[[backend]] <- prepClimateLayers(
        climateVarsList = climateVariables,
        srcdir = climatePath, ## 'src' is the place for raw inputs, downloaded from Google Drive
        dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
        tile = tileIDs,
        gcm = GCM,
        ssp = SSP,
        cl = NULL,
        studyArea = NULL,
        studyAreaName = NULL,
        rasterToMatch = NULL
      )
    })

    lapply(climateRasters[[backend]], function(x) {
      expect_s4_class(x, "SpatRaster")
    })

    #new climate variables

    climateVariables <- list(
      historical_PAS_normal = list(
        vars = "historical_PAS_normal", ## ensure MAT only; not bMAT nor eMAT
        fun = quote(calcAsIs),
        .dots = list(historical_period = historical_prd)
      ),
      historical_CMD_normal = list(
        vars = "historical_CMD_normal", ## ensure MAT only; not bFFP nor eFFP
        fun = quote(calcAsIs),
        .dots = list(historical_period = historical_prd)
      )
    )

    climateRasters <- list()
    for (backend in .parallelBackends) {
      withr::with_options(list(climateData.parallel.backend = backend), {
        climateRasters[[backend]] <- prepClimateLayers(
          climateVarsList = climateVariables,
          srcdir = climatePath, ## 'src' is the place for raw inputs, downloaded from Google Drive
          dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
          tile = tileIDs,
          gcm = GCM,
          ssp = SSP,
          cl = NULL,
          studyArea = NULL,
          studyAreaName = NULL,
          rasterToMatch = NULL
        )
      })

      lapply(climateRasters[[backend]], function(x) {
        expect_s4_class(x, "SpatRaster")
      })

      ras1a <- climateRasters[[1]][[1]][[1]]
      ras1b <- climateRasters[[1]][[1]][[2]]
      ras2a <- climateRasters[[1]][[2]][[1]]

      #avoid false just because of layer name
      names(ras1a) <- names(ras1b)
      names(ras2a) <- names(ras1a)

     #check same var diff normal period
      expect_false(isTRUE(terra::all.equal(ras1a, ras1b)))
     #check diff var same normal period
      expect_false(isTRUE(terra::all.equal(ras1a, ras2a)))
    }

  ## now test with studyArea and rasterTaMatch
  skip_if_not_installed("SpaDES.tools")

  studyArea <- SpaDES.tools::randomStudyArea(size = 1e10)
  rasterToMatch <- terra::rast(studyArea, resolution = 250) |> terra::rasterize(studyArea, y = _)

  ## test with all parallel backends
  climateRasters <- list()
  for (backend in .parallelBackends) {
    withr::with_options(list(climateData.parallel.backend = backend), {
      ## spurious warning:
      ## attribute variables are assumed to be spatially constant throughout all geometries
      climateRastersStudyArea <- suppressWarnings({
        prepClimateLayers(
          climateVarsList = climateVariables,
          srcdir = climatePath, ## 'src' is the place for raw inputs, downloaded from Google Drive
          dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
          # tile = tileIDs, ## when passing `studyArea`/`rasterToMatch` then `tile` isn't needed
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
    })
  }

  withr::deferred_run()
})

test_that("prepClimateLayers properly handles unordered tileIDs", {
  skip_on_cran()
  skip_on_ci() ## needs to authorize googldrive package for downloads
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")
  skip_if_not_installed("withr")

  dPath <- withr::local_tempdir("test_prepClimateLayers_")
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
    srcdir = climatePath, ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    tile = tileIDs,
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

  withr::deferred_run()
})

