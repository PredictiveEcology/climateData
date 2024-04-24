#' Determine MSYN type of a climate variable
#'
#' @template ClimateNA_climVars
#'
#' @export
#' @rdname climVarTypes
whichMSYN <- function(climVars) {
  vapply(climVars, function(x) {
    if (grepl("_normal$", x)) {
      "N" ## all normals are yearly/annual?
    } else if (grepl(".*_(wt|sp|sm|at)$", x)) {
      "S"
    } else  if (grepl(".*[0-1][1-9]$", x)) {
      "M"
    } else {
      "Y"
    }
  }, character(1))
}

#' @export
#' @importFrom dplyr first
#' @rdname climVarTypes
whichTypes <- function(climVars) {
  vapply(climVars, function(x) {
    type <- strsplit(x, "_") |> unlist() |> dplyr::first()
    type <- ifelse(grepl("_normal$", x), paste0(type, "_normal"), type)
    return(type)
  }, character(1))
}

#' Prepare rasters for derived and 'as-is' climate variables
#'
#' TODO
#'
#' @param climateVarsList TODO
#'
#' @template ClimateNA_srcdstdir
#'
#' @template ClimateNA_tile
#'
#' @template ClimateNA_typeyears
#'
#' @template ClimateNA_typeperiod
#'
#' @template ClimateNA_gcmssp
#'
#' @template cl
#'
#' @template studyArea
#'
#' @param studyAreaName character string giving  the name of the study area.
#'
#' @template rasterToMatch
#'
#' @param currentModuleName TODO
#'
#' @param ... additional arguments (not used)
#'
#' @export
#' @importFrom dplyr first last
#' @importFrom purrr transpose
#' @importFrom reproducible .robustDigest Cache postProcessTo
#' @importFrom sf st_union
#' @importFrom terra aggregate rast set.names
#'
#' @examples
#' if (require("archive", quietly = TRUE) &&
#'     require("googledrive", quietly = TRUE) &&
#'     require("SpaDES.tools", quietly = TRUE) &&
#'     !isTRUE(as.logical(Sys.getenv("CI", "false")))) {
#'   dPath <- file.path(tempdir(), "test_prepClimateLayers")
#'   climateType <- "historical"
#'   climatePath <- file.path(dPath, "climate") |> reproducible::checkPath(create = TRUE)
#'   climatePathOut <- file.path(climatePath, "outputs") |> reproducible::checkPath(create = TRUE)
#'   historical_prd <- c("1951_1980", "1981_2010")
#'   historical_yrs <- c(2011:2015)
#'   future_yrs <- c(2021:2025)
#'   GCM <- "CanESM5"
#'   SSP <- 370
#'   studyArea <- SpaDES.tools::randomStudyArea(size = 1e10)
#'   rasterToMatch <- terra::rast(studyArea, resolution = 250) |>
#'     terra::rasterize(studyArea, y = _)
#'
#'   ## ATA mixes seasonal and normal variables; uses custom fun
#'   ## CMI uses yearly variable; no fun (as is)
#'   ## FFP uses yearly  variable; no fun (as is); similar name to bFFP and eFFP
#'   ## MDC uses monthly vars; uses custom fun
#'   climateVariables <- list(
#'     historical_ATA = list(
#'       vars = c("historical_MAT", "historical_MAT_normal"),
#'       fun = quote(calcATA),
#'       .dots = list(historical_period = historical_prd, historical_years = historical_yrs)
#'     ),
#'     future_ATA = list(
#'       vars = c("future_MAT", "historical_MAT_normal"),
#'       fun = quote(calcATA),
#'       .dots = list(historical_period = historical_prd, future_years = future_yrs)
#'     ),
#'     historical_CMI = list(
#'       vars = "historical_CMI",
#'       fun = quote(calcAsIs),
#'       .dots = list(historical_years = historical_yrs)
#'     ),
#'     historical_CMI_normal = list(
#'       vars = "historical_CMI_normal",
#'       fun = quote(calcCMInormal),
#'       .dots = list(historical_period = historical_prd, historical_years = historical_yrs)
#'     ),
#'     future_FFP = list(
#'       vars = "future_FFP", ## ensure FFP only; not bFFP nor eFFP
#'       fun = quote(calcAsIs),
#'       .dots = list(future_years = future_yrs)
#'     ),
#'     historical_MDC = list(
#'       vars = c(sprintf("historical_PPT%02d", 4:9), sprintf("historical_Tmax%02d", 4:9)),
#'       fun = quote(calcMDC),
#'       .dots = list(historical_years = historical_yrs)
#'     )
#'   )
#'
#'   climateRasters <- prepClimateLayers(
#'     climateVarsList = climateVariables,
#'     srcdir = climatePath,    ## raw inputs, downloaded from Google Drive
#'     dstdir = climatePathOut, ## intermediate + final outputs
#'     historical_years = historical_yrs,
#'     future_years = future_yrs,
#'     historical_period = historical_prd,
#'     future_period = NULL,
#'     gcm = GCM,
#'     ssp = SSP,
#'     cl = NULL,
#'     studyArea = studyArea,
#'     studyAreaName = "my_study_area",
#'     rasterToMatch = rasterToMatch
#'   )
#' }
prepClimateLayers <- function(climateVarsList, srcdir, dstdir,
                               tile = NULL,
                               future_years = NULL, future_period = NULL,
                               historical_years = NULL, historical_period = NULL,
                               gcm = NULL, ssp = NULL, cl = NULL,
                               studyArea = NULL, studyAreaName = NULL, rasterToMatch = NULL,
                               currentModuleName = "NoModule", ...) {
  stopifnot(
    !missing(srcdir), !missing(dstdir),
    !all(is.null(future_years), is.null(future_period), is.null(historical_years), is.null(historical_period)),
    (!is.null(tile) && is.null(studyArea) && is.null(rasterToMatch)) || ## pass tile but not sA/RTM
      (is.null(tile) && !is.null(studyArea) && !is.null(rasterToMatch)) ## pass sA/RTM but not tile
  )

  climatePath <- srcdir
  climatePathOut <- dstdir

  ## need full study area perimeter w/o subpolygons
  if (!is.null(studyArea)) {
    if (is(studyArea, "sf")) {
      studyArea <- sf::st_union(studyArea)
    } else if (is(studyArea, "SpatVector")) {
      studyArea <- terra::aggregate(studyArea)
    }
  }

  if (is.null(tile) && !is.null(studyArea) && !is.null(rasterToMatch)) {
    tile <- whereAmI(studyArea)
  }

  ## determine which climate vars are needed from climateVarsList$newVar$var
  # annual vars: XXX
  # monthly vars: XXX00
  # seasonal vars: XXX_zz
  # normal vars: will need to be prefixed with 'normal_'
  needVars <- purrr::transpose(climateVarsList)[["vars"]] |>
    unlist() |>
    unname() |>
    unique()
  types <- whichTypes(needVars)
  MSYN <- whichMSYN(needVars)
  unique_types_msy <- paste0(types, "_", MSYN) |> gsub("_normal", "", x = _) |> unique()
  ## TODO: for now, seasonal variables are only available in the MSY ("all") archives
  unique_types_msy <- gsub("^historical_S$", "historical_MSY", unique_types_msy) |> unique()
  unique_types_msy <- gsub("^future_S$", "future_MSY", unique_types_msy) |> unique()

  ## consolidate types -- if all needed, download all variables in same archive
  if (all(c("historical_M", "historical_S", "historical_Y") %in% unique_types_msy)) {
    unique_types_msy <- gsub("^historical_(M|S|Y)$", "historical_MSY", unique_types_msy) |> unique()
  }
  if (all(c("future_M", "future_S", "future_Y") %in% unique_types_msy)) {
    unique_types_msy <- gsub("^future_(M|S|Y)$", "future_MSY", unique_types_msy) |> unique()
  }

  ## 2. download and extract multiple archives per tile (by decade)
  climPreProcessOut <- lapply(unique_types_msy, function(type_msyn) {
    if (type_msyn %in% c("historical_M", "historical_S", "historical_Y", "historical_MSY")) {
      type <- "historical"
      msy <- strsplit(type_msyn, "historical_")[[1]] |> dplyr::last()
      climatePath_type <- file.path(climatePath, type)
      urls <- getClimateURLs(type = type, tile = tile, years = historical_years, msy = msy)
      out <- getClimateTiles(tile = tile, climateURLs = urls, climatePath = climatePath_type)
      allDirs <- file.path(climatePath_type, rep(tile, length(historical_years)),
                           paste0("Year_", historical_years, msy)) |> sort()
    } else if (type_msyn == "historical_N") {
      type <- "historical_normals"
      climatePath_type <- file.path(climatePath, "historical", "normals")
      urls <- getClimateURLs(type = type, tile = tile, msy = "Y")
      out <- getClimateTiles(tile = tile, climateURLs = urls, climatePath = climatePath_type)
      allDirs <- file.path(climatePath_type, rep(tile, length(historical_period)),
                           paste0("Normal_", historical_period, "Y")) |> sort()
    } else if (type_msyn %in% c("future_M", "future_S", "future_Y", "future_MSY")) {
      type <- "future"
      msy <- strsplit(type_msyn, "future_")[[1]] |> dplyr::last()
      climatePath_type <- file.path(climatePath, type)
      urls <- getClimateURLs(type = type, tile = tile, years = future_years, msy = msy, gcm = gcm, ssp = ssp)
      out <- getClimateTiles(tile = tile, climateURLs = urls, climatePath = climatePath_type)
      allDirs <- file.path(climatePath_type, rep(tile, length(future_years)),
                           paste0(gcm, "_ssp", ssp, "@", future_years, msy)) |> sort()
    } else if (type_msyn == "future_N") {
      stop("future normals not yet implemented") ## TODO: get data, implement and verify
      type <- "future_normals"
      climatePath_type <- file.path(climatePath, "future", "normals")
      urls <- getClimateURLs(type = type, tile = tile, msy = "Y", gcm = gcm, ssp = ssp)
      out <- getClimateTiles(tile = tile, climateURLs = urls, climatePath = climatePath_type)
      allDirs <- file.path(climatePath_type, rep(tile, length(future_period)), paste0(gcm, "_ssp", ssp),
                           paste0("Normal_", future_period, "Y")) |> sort() ## TODO: verify paths
    } else {
      stop("unknown climate variable type: ", type_msyn)
    }

    stopifnot(all(dir.exists(allDirs)))

    return(out)
  })

  ## 3. load data into R -- build vrts and write mosaic tifs to disk
  ##    these are **unprojected** tile mosaics and will need to be postProcessed

  ## historical
  vars_hist_msy <- grep("^historical_", needVars, value = TRUE) |>
    grep("_normal$", x = _, value = TRUE, invert = TRUE) |>
    gsub("^historical_", "", x = _) ## strip prefix

  if (length(vars_hist_msy) > 0) {
    tifs_hist_msy <- buildClimateMosaics(
      type = "historical",
      tile = tile,
      climVars = vars_hist_msy,
      years = historical_years,
      srcdir = file.path(climatePath, "historical"),
      dstdir = climatePathOut,
      cl = cl
    )
  } else {
    tifs_hist_msy <- list()
  }

  ## historical_normals
  vars_hist_nrm <- grep("^historical_", needVars, value = TRUE) |>
    grep("_normal$", x = _, value = TRUE) |>
    gsub("^historical_", "", x = _) ## strip prefix

  if (length(vars_hist_nrm) > 0) {
    tifs_hist_nrm <- buildClimateMosaicsNormals(
      type = "historical",
      tile = tile,
      climVars = vars_hist_nrm,
      period = historical_period,
      srcdir = file.path(climatePath, "historical", "normals"),
      dstdir = climatePathOut,
      cl = cl
    )
  } else {
    tifs_hist_nrm <- list()
  }

  ## future
  vars_futu_msy <- grep("^future_", needVars, value = TRUE) |>
    grep("_normal$", x = _, value = TRUE, invert = TRUE) |>
    gsub("^future_", "", x = _) ## strip prefix

  if (length(vars_futu_msy) > 0) {
    tifs_futu_msy <- buildClimateMosaics(
      type = "future",
      tile = tile,
      climVars = vars_futu_msy,
      years = future_years,
      gcm = gcm,
      ssp = ssp,
      srcdir = file.path(climatePath, "future"),
      dstdir = climatePathOut,
      cl = cl
    )
  } else {
    tifs_futu_msy <- list()
  }

  ## future_normals
  vars_futu_nrm <- grep("^future_", needVars, value = TRUE) |>
    grep("_normal$", x = _, value = TRUE) |>
    gsub("^future_", "", x = _) ## strip prefix

  if (length(vars_futu_nrm) > 0) {
    tifs_futu_nrm <- buildClimateMosaicsNormals(
      type = "future",
      tile = tile,
      climVars = vars_futu_nrm,
      period = future_period,
      gcm = gcm,
      ssp = ssp,
      srcdir = file.path(climatePath, "future", "normals"),
      dstdir = climatePathOut,
      cl = cl
    )
  } else {
    tifs_futu_nrm <- list()
  }

  ## TODO: is this strictly necessary; wouldn't GDAL buildvrt fail during buildClimateMosaics()?
  tifs_msy <- append(tifs_hist_msy, tifs_futu_msy)
  tifs_nrm <- append(tifs_hist_nrm, tifs_futu_nrm)
  tifs <- append(tifs_msy, tifs_nrm)
  if (!all(file.exists(unlist(tifs)))) {
    stop("The following mosaic rasters could not be found:\n",
         paste(basename(tifs[!file.exists(tifs)]), collapse = ", "), ".\n",
         "Please check that all source data files are present in ", dstdir, ".")
  }

  ## ClimateNA v7.41 (August 01, 2023) release note:
  ## Climate variables with decimals are no longer converted to integers in raster format.
  climStacks_historical_MSY <- climateStacksByYear(tifs = tifs_hist_msy, climVars = vars_hist_msy,
                                                 type = "historical", years = historical_years)

  climStacks_historical_N <- climateStacksByPeriod(tifs = tifs_hist_nrm, climVars = vars_hist_nrm,
                                                 type = "historical", period = historical_period)

  climStacks_future_MSY <- climateStacksByYear(tifs = tifs_futu_msy, climVars = vars_futu_msy,
                                               type = "future", years = future_years)

  climStacks_future_N <- climateStacksByPeriod(tifs = tifs_futu_nrm, climVars = vars_futu_nrm,
                                               type = "future", period = future_period)

  climStacks <- c(climStacks_historical_MSY, climStacks_future_MSY,
                  climStacks_historical_N, climStacks_future_N)

  ## 4. do `climateVarsList$newVar$fun` to the tifs (still **unprojected**)
  climDataFun <- lapply(climateVarsList, function(newClimVar) {
    climVars <- newClimVar[["vars"]]
    climFun <- newClimVar[["fun"]]
    climDots <- newClimVar[[".dots"]]

    funOut <- do.call(eval(climFun), list(
      stacks = climStacks,
      layers = climVars,
      .dots = climDots
    ))

    return(funOut)
  })

  ## 5. post-process (i.e., GIS) the now-whole rasters for the studyArea
  if (is.null(studyAreaName)) {
    studyAreaName <- paste0("tiles_", paste0(tile, collapse = "-"))
  }

  digest4cache <- c("climatePathOut", "currentModuleName",
                    "future_years", "future_period", "historical_years", "historical_period",
                    "rasterToMatch", "studyArea", "studyAreaName") |>
    mget(envir = environment()) |>
    .robustDigest(object = _)

  climData <- lapply(names(climDataFun), function(nm) {
    newClimRast <- climDataFun[[nm]]
    type <- strsplit(nm, "_")[[1]] |> dplyr::first()
    var <- strsplit(nm, "_")[[1]] |> dplyr::last()
    fname <- paste(var, type, studyAreaName, sep = "_")
    climRast <- Cache(
      postProcessTo(
        from = newClimRast,
        to = rasterToMatch,
        maskTo = studyArea,
        writeTo = file.path(climatePathOut, paste0(fname, ".tif")),
        useCache = FALSE, ## use internal cache for postProcessTo
        overwrite = TRUE
      ),
      omitArgs = c("to", "maskTo", "overwrite"), # don't digest these each time
      .functionName = paste0("prepClimateLayers_", fname),
      .cacheExtra = digest4cache,
      quick = c("writeTo", "climatePath", "climatePathOut"), # don't cache on outputs
      userTags = c(paste0(type[1]), fname)
    )
    # terra::time(climRast, tstep = "year") <- years ## TODO: what about monthly?

    return(climRast)
  })
  names(climData) <- names(climDataFun) ## TODO: verify

  return(climData)
}
