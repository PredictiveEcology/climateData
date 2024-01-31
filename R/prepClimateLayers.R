#' Prepare climate rasters
#'
#' TODO
#'
#' @param climateVarsList TODO
#'
#' @template ClimateNA_srcdstdir
#'
#' @template ClimateNA_tile
#'
#' @template ClimateNA_years
#'
#' @template ClimateNA_type
#'
#' @template cl
#'
#' @template studyArea
#'
#' @param studyAreaName TODO
#'
#' @param rasterToMatch TODO
#'
#' @param currentModuleName TODO
#'
#' @param ... TODO
#'
#' @export
#' @importFrom purrr transpose
#' @importFrom reproducible .robustDigest
#' @importFrom terra set.names rast
prepClimateRasters <- function(climateVarsList,
                               srcdir, dstdir, tile = NULL, years = NULL, type = NULL, cl = NULL,
                               studyArea = NULL, studyAreaName = NULL, rasterToMatch = NULL,
                               currentModuleName = "NoModule", ...) {

  stopifnot(
    !missing(srcdir), !missing(dstdir), !is.null(years), !is.null(type),
    (!is.null(tile) && is.null(studyArea) && is.null(rasterToMatch)), ## pass tile but not sA/RTM
    (is.null(tile) && !is.null(studyArea) && !is.null(rasterToMatch)), ## pass sA/RTM but not tile
    type %in% c("historic", "future")
  )

  type_normals <- paste0(type, "_normals")
  climatePath <- srcdir
  climatePath_type <- file.path(climatePath, type)
  climatePath_normals <- file.path(climatePath_type, "normals")

  climatePathOut <- dstdir

  ## TODO: need digestSA_RTM
  digest4cache <- c("climatePathOut", "currentModuleName",
                    "rasterToMatch", "studyArea", "studyAreaName",
                    "type", "years") |>
    mget(envir = environment()) |>
    .robustDigest(object = _)

  ## 1. determine which climate vars are needed from climateVarsList$newVar$var
  # annual vars: XXX
  # monthly vars: XXX00
  # seasonal vars: XXX_zz
  # normal vars: will need to be prefixed with 'normal_'
  needVars <- purrr::transpose(climateVarsList)[["vars"]] |>
    unlist() |>
    unname() |>
    unique()
  MSYN <- "Y" ## assume always need yearly variables
  if (any(grepl(".*_(wt|sp|sm|at)$", needVars))) {
    MSYN <- c(MSYN, "S")
  }
  if (any(grepl(".*[0-1][1-9]$", needVars))) {
    MSYN <- c(MSYN, "M")
  }
  if (all((c("M", "S", "Y") %in% MSYN)) || ("S" %in% MSYN)) {
    MSYN = "MSY"
  }
  if (any(grepl("^normal_", needVars))) {
    MSYN <- c(MSYN, "N")
  }

  ## 2. download and extract multiple archives per tile (by decade)
  climPreProcessOut <- lapply(MSYN, function(msy) {
    if (msy == "N") {
      ## type historic_normals or future_normals (Y)
      urls <- getClimateURLs(type = type_normals, tile = tile, years = years)
      out <- getClimateTiles(tile = tile, climateURLs = urls, climatePath = climatePath_normals)
      allDirs <- file.path(climatePath_normals, rep(tile, length(years)), paste0("Year_", years, "Y")) |> sort()
    } else {
      ## type historic or future (MSY)
      urls <- getClimateURLs(type = type, tile = tile, years = years, msy = msy)
      out <- getClimateTiles(tile = tile, climateURLs = urls, climatePath = climatePath_type)
      allDirs <- file.path(climatePath_type, rep(tile, length(years)), paste0("Year_", years, msy)) |> sort()
    }
    stopifnot((all(dir.exists(allDirs))))

    return(out)
  })

  ## 3. load data into R -- build vrts and write mosaic tifs to disk
  ##    these are **unprojected** tile mosaics and will need to be postProcessed
  tifs <- buildClimateMosaics(
    tile = tile,
    climVars = needVars,
    years = years,
    srcdir = climatePath_type,
    dstdir = climatePathOut,
    cl = cl
  )

  ## TODO: is this strictly necessary; wouldn't GDAL buildvrt fail during buildClimateMosaics()?
  if (!all(file.exists(unlist(tifs)))) {
    stop("The following mosaic rasters could not be found:\n",
         paste(basename(tifs[!file.exists(tifs)]), collapse = ", "), ".\n",
         "Please check that all source data files are present in ", dstdir, ".")
  }

  ## ClimateNA v7.41 (August 01, 2023) release note:
  ## Climate variables with decimals are no longer converted to integers in raster format.
  climStacks <- lapply(as.character(years), function(y) {
    rs <- lapply(tifs[[y]], function(files) {
      r <- terra::rast(files)
      shortName <- vapply(basename(files), function(f) strsplit(f, "_")[[1]][1], character(1))
      terra::set.names(r, shortName)

      return(r)
    }) |>
      terra::rast()
    terra::set.names(rs, needVars)

    return(rs)
  })
  names(climStacks) <- as.character(years)

  ## 4. do `climateVarsList$newVar$fun` to the tifs (still **unprojected**)
  climDataFun <- lapply(climateVarsList, function(newClimVar) {
    climVars <- newClimVar$vars
    climFun <- newClimVar$fun
    funOut <- do.call(eval(climFun), list(
      stacks = climStacks,
      layers = climVars
      ## TODO: use .dots to pass through additional args?
    ))

    return(funOut)
  })

  ## 5. post-process (i.e., GIS) the now-whole rasters for the studyArea
  if (is.null(studyAreaName)) {
    studyAreaName <- paste0("tiles_", paste0(tile, collapse = "-"))
  }

  climData <- lapply(climDataFun, function(newClimRast) {
    fname <- paste0(strsplit(names(newClimRast), "_")[[1]][1],
                    "_", type[1], "_", studyAreaName)
    climRast <- Cache(
      postProcessTo(
        from = newClimRast,
        to = rasterToMatch,
        maskTo = studyArea,
        writeTo = file.path(climatePathOut, paste0(fname, ".tif")),
        # datatype = "INT2U",
        useCache = FALSE, ## use internal cache for postProcessTo
        overwrite = TRUE # overwrite so it doesn't require manual intervention
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

  return(climData)
}
