test_that("prepClimateLayers works for multiple variable types", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("googledrive")

  skip_if_not(interactive())

  dPath <- file.path(tempdir(), "test_prepClimateLayers")
  climateType <- "historic"
  climatePath <- file.path(dPath, "climate") |> reproducible::checkPath(create = TRUE)
  climatePathOut <- file.path(climatePath, "outputs") |> reproducible::checkPath(create = TRUE)
  tileIDs <- c(16, 17, 26, 27)
  yrs <- c(2011:2015)

  ## ATA mixes seasonal and normal variables; uses custom fun
  ## CMD_sm uses seasonal variable; no fun (as is)
  ## MDC uses monthly vars; uses custom fun
  historicalClimateVariables <- list(
    # ATA = list(vars = c("Tmax_sm", "normal_Tmax"),  FUN = quote(calcATA(...))),
    # CMD_sm = list(vars = c("CMD_sm"), FUN = NULL),
    MDC = list(vars = c(sprintf("PPT%02d", 4:9), sprintf("Tmax%02d", 4:9)),
               fun = quote(calcMDC))
  )

  historicalClimateRasters <- prepClimateRasters(
    climateVarsList = historicalClimateVariables,
    srcdir = climatePath,    ## 'src' is the place for raw inputs, downloaded from Google Drive
    dstdir = climatePathOut, ## 'dst' is the place for intermediate + final outputs
    tile = tileIDS,
    years = yrs,
    type = "historic",
    cl = NULL,
    studyArea = NULL, ## TODO: if passing `studyArea`/`rasterToMatch` then `tile` isn't needed
    studyAreaName = NULL,
    rasterToMatch = NULL
  )

  lapply(historicalClimateRasters, function(x) {
    expect_s4_class(x, "SpatRaster")
  })
  # expect_identical(names(mdc), paste0("mdc", yrs))

  allDirs <- file.path(climatePath, rep(tileIDs, length(yrs)), paste0("Year_", yrs, "M")) |> sort()
  expect_true(all(dir.exists(allDirs)))

  ## TODO: test with studyArea and rasterTaMatch

  unlink(dPath, recursive = TRUE)
})
