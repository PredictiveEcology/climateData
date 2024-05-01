utils::globalVariables(c(
  "gid", "period", "tileid", "year"
))

#' Identify ClimateNA tiles overlapping a study area
#'
#' @template studyArea
#'
#' @return integer. vector of tile IDs.
#'
#' @export
#' @importFrom reproducible postProcessTo
#' @importFrom sf st_read
whereAmI <- function(studyArea) {
  sf::st_read(ClimateNA_tiles_gpkg(), quiet = TRUE) |>
    reproducible::postProcessTo(from = _, to = studyArea) |>
    (function(x) x$id)()
}

#' Extract a table from the ClimateNA sqlite databose
#'
#' @template ClimateNA_type
#' @template ClimateNA_tile
#' @template ClimateNA_years
#' @template ClimateNA_msy
#' @template ClimateNA_gcmssp
#'
#' @return `data.frame`
#'
#' @export
#' @importFrom DBI dbDisconnect
#' @importFrom dplyr collect filter
#'
#' @examples
#' (getClimateTable(type = "historical", tile = 1, msy = "M", years = 2011:2020))
#' (getClimateTable(type = "future", tile = 1, msy = "Y", years = 2051:2060,
#'                  gcm = "CanESM5", ssp = 370))
#' (getClimateTable(type = "historical_normals", tile = 1, msy = "Y"))
#' (getClimateTable(type = "future_normals", tile = 1, msy = "Y",
#'                  gcm = "CanESM5", ssp = 370)) ## none yet available
getClimateTable <- function(type = NULL, tile = NULL, years = NULL, msy = NULL,
                            gcm = NULL, ssp = NULL) {
  stopifnot(
    type %in% .allowedClimateTypes,
    !is.null(tile),
    !is.null(msy)
  )

  if (isTRUE(grepl("future", type))) {
    stopifnot(!is.null(gcm), !is.null(ssp))
  }

  if (isFALSE(grepl("normals", type))) {
    stopifnot(!is.null(years))
  }

  dbdf <- ClimateNA_sql(ClimateNA_tiles_sqlite(), type)
  climate_db <- dbdf[["db"]]
  climate_df <- dbdf[["df"]]
  on.exit(DBI::dbDisconnect(climate_db), add = TRUE)

  climate_dt <- if (type == "historical") {
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy, year %in% !!years)
  } else if (type == "future") {
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy, year %in% !!years,
                  gcm == !!gcm, ssp == !!as.character(ssp))
  } else if (type == "historical_normals") {
    ## all periods in single zip
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy)
  } else if (type == "future_normals") {
    ## all periods in single zip per gcm_ssp
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy,
                  gcm == !!gcm, ssp == !!as.character(ssp))
  }
  climate_dt <- dplyr::collect(climate_dt)
}

#' Get historical or future climate normals periods
#'
#' @template ClimateNA_type
#' @template ClimateNA_tile
#' @template ClimateNA_msy
#' @template ClimateNA_gcmssp
#'
#' @return character.
#'
#' @export
#' @importFrom dplyr pull
getNormalsPeriods <- function(type = NULL, tile = NULL, msy = "Y", gcm = NULL, ssp = NULL) {
  if (type %in% c("historical", "future")) {
    type <- paste0(type, "_normals")
  }

  getClimateTable(type = type, tile = tile, msy = msy, gcm = gcm, ssp = ssp) |>
    dplyr::pull(period) |>
    unique()
}

#' Look up the URLs for a set of climate tiles
#'
#' @template ClimateNA_type
#' @template ClimateNA_tile
#' @template ClimateNA_years
#' @template ClimateNA_msy
#' @template ClimateNA_gcmssp
#'
#' @return list of Google Drive ids, with names corresponding the tile ids.
#'
#' @export
#' @importFrom data.table .SD as.data.table
getClimateURLs <- function(type = NULL, tile = NULL, years = NULL, msy = NULL,
                           gcm = NULL, ssp = NULL) {
  climate_dt <- getClimateTable(type = type, tile = tile, years = years, msy = msy,
                                gcm = gcm, ssp = ssp) |>
    as.data.table()
  subsetdt <- climate_dt[, .SD, .SDcols = c("tileid", "gid")] |> unique()
  url <- lapply(unique(subsetdt$tileid), function(t) {
    subsetdt[tileid == t, gid]
  })
  names(url) <- unique(subsetdt$tileid)

  return(url)
}

#' Download and extract multiple archives per tile (by decade)
#'
#' @template ClimateNA_tile
#'
#' @param climateURLs list of lists of tile URLs, with outer list names corresponding to tile ids.
#'
#' @param climatePath character string specifying the directory path to put climate data.
#'
#' @return (invisibly) a list of length `tileIDs` containing the result of `preProcess()` calls
#'
#' @export
#' @importFrom reproducible preProcess
getClimateTiles <- function(tile, climateURLs, climatePath) {
  stopifnot(requireNamespace("googledrive", quietly = TRUE))

  Map(
    climateTile = tile,
    climateURL = climateURLs,
    function(climateTile, climateURL) {
      ## TODO: the zip files are being put inside the tile directory, but should be one level up
      preProcessOut <- lapply(climateURL, function(url) {
        preProcess(
          url = googledrive::as_id(url),
          targetFile = NULL,
          destinationPath = file.path(climatePath, climateTile) ## keep tile dir structure
        ) ## TODO: how to best use Cache here?
      })
      names(preProcessOut) <- paste0("tile_", climateTile)

      return(preProcessOut)
    }
  ) |>
    invisible()
}

#' Build climate mosaic rasters from ClimateNA tiles
#'
#' @template ClimateNA_type
#'
#' @template ClimateNA_tile
#'
#' @template ClimateNA_climVars
#'
#' @template ClimateNA_years
#'
#' @template ClimateNA_gcmssp
#'
#' @template ClimateNA_srcdstdir
#'
#' @template cl
#'
#' @return character. filepaths corresponding to the raster mosaic files for each `climVar`.
#'         invoked for the side effect of creating these raster mosaics on disk.
#'
#' @export
#' @importFrom fs dir_ls
#' @importFrom parallel parLapply stopCluster
#' @importFrom parallelly availableCores
#' @importFrom reproducible checkPath
#' @importFrom sf gdal_utils
#' @importFrom tools file_path_sans_ext
#'
#' @rdname buildClimateMosaics
buildClimateMosaics <- function(type, tile, climVars, years, gcm = NULL, ssp = NULL,
                                srcdir = NULL, dstdir = NULL, cl = NULL) {

  if (any(grepl("_normal$", climVars))) {
    stop("At least one of 'climVars' is a climate normal variable; ",
         "use buildClimateMosaicsNormals() for these.")
  }

  if (any(is.null(srcdir), is.null(dstdir))) {
    stop("both 'srcdir' and 'dstdir' must specified and non-NULL.")
  }

  srcdir <- checkPath(srcdir, create = TRUE)
  dstdir <- checkPath(dstdir, create = TRUE)

  cores <- min(length(years), parallelly::availableCores(constraints = "connections"))

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(cores,
                                       default_packages = c("fs", "sf", "terra"),
                                       rscript_libs = .libPaths(),
                                       autoStop = TRUE)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  parallel::clusterExport(cl, c("climVars", "dstdir", "srcdir", "tile"), envir = environment())

  tifs <- parallel::parLapply(cl, years, function(y) {
    lapply(climVars, function(v) {
      srcfiles <- srcdir |>
        fs::dir_ls(regexp = paste0(tile, "$", collapse = "|"), type = "directory") |>
        fs::dir_ls(regexp = y, type = "directory") |>
        fs::dir_ls(regexp = paste0("(/|\\\\)", v, "[.]asc$"), type = "file")

      stopifnot(length(srcfiles) > 0)

      ## vrts must be a single layer, so need to do e.g. monthly climVars by each month
      lapply(unique(basename(srcfiles)), function(mv) {
        mv <- tools::file_path_sans_ext(mv)
        fname <- paste(mv, basename(basename(srcdir)), y, "t", paste(tile, collapse = "-"), sep = "_")
        dstvrt <- file.path(tempdir(), paste0(fname, ".vrt"))
        dsttif <- file.path(dstdir, paste0(fname, ".tif"))

        ## TODO: by default, layer name is filename; update this to be `mv`
        sf::gdal_utils(util = "buildvrt", source = srcfiles, destination = dstvrt)
        sf::gdal_utils(util = "warp", source = dstvrt, destination = dsttif)

        on.exit(file.remove(dstvrt), add = TRUE)

        return(dsttif)
      }) |>
        unlist()
    }) |>
      unlist()
  }) ## don't flatten the out list; want to access list of climate variables per year
  names(tifs) <- paste0(type, "_", years)

  return(tifs)
}

#' @template ClimateNA_period
#'
#' @export
#' @rdname buildClimateMosaics
buildClimateMosaicsNormals <- function(type, tile, climVars, period, gcm = NULL, ssp = NULL,
                                       srcdir = NULL, dstdir = NULL, cl = NULL) {

  if (!all(grepl("_normal$", climVars))) {
    stop("At least one of 'climVars' is not a climate normal variable; ",
         "use buildClimateMosaics() for these.")
  }

  if (any(is.null(srcdir), is.null(dstdir))) {
    stop("both 'srcdir' and 'dstdir' must specified and non-NULL.")
  }

  srcdir <- checkPath(srcdir, create = TRUE)
  dstdir <- checkPath(dstdir, create = TRUE)

  cores <- min(length(period), parallelly::availableCores(constraints = "connections"))

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(cores,
                                       default_packages = c("fs", "sf", "terra"),
                                       rscript_libs = .libPaths(),
                                       autoStop = TRUE)
  }

  parallel::clusterExport(cl, c("climVars", "dstdir", "srcdir", "tile"), envir = environment())

  tifs <- parallel::parLapply(cl, period, function(p) {
    lapply(climVars, function(v) {
      nv <- strsplit(v, "_normal")[[1]]
      srcfiles <- srcdir |>
        fs::dir_ls(regexp = paste0(tile, "$", collapse = "|"), type = "directory") |>
        fs::dir_ls(regexp = p, type = "directory") |>
        fs::dir_ls(regexp = paste0(nv, ".*[.]asc$"), type = "file") ## TODO: improve regex; e.g., bFFP, eFFP vs FFP

      stopifnot(length(srcfiles) > 0)

      ## vrts must be a single layer, so need to do e.g. monthly climVars by each month
      lapply(unique(basename(srcfiles)), function(mv) {
        mv <- tools::file_path_sans_ext(mv)
        fname <- paste(mv, basename(basename(srcdir)), p, "t", paste(tile, collapse = "-"), sep = "_")
        dstvrt <- file.path(tempdir(), paste0(fname, ".vrt"))
        dsttif <- file.path(dstdir, paste0(fname, ".tif"))

        ## TODO: by default, layer name is filename; update this to be `mv`
        sf::gdal_utils(util = "buildvrt", source = srcfiles, destination = dstvrt)
        sf::gdal_utils(util = "warp", source = dstvrt, destination = dsttif)

        on.exit(file.remove(dstvrt), add = TRUE)

        return(dsttif)
      }) |>
        unlist()
    }) |>
      unlist()
  }) ## don't flatten the out list; want to access list of climate variables per year
  names(tifs) <- paste0(type, "_", period)

  return(tifs)
}


#' Climate Stacks by Year
#'
#' @param tifs TODO
#'
#' @template ClimateNA_type
#'
#' @template ClimateNA_climVars
#'
#' @template ClimateNA_years
#'
#' @return list of `SpatRaster`s
#'
#' @export
#' @importFrom terra rast set.names
#' @rdname climateStacksByYear
climateStacksByYear <- function(tifs, climVars, type, years) {
  if (length(tifs) > 0) {
    namesPerYear <- paste0(type, "_", years)
    climStacks_msy <- lapply(namesPerYear, function(y) {
      rs <- lapply(tifs[[y]], function(files) {
        r <- terra::rast(files)
        shortName <- vapply(basename(files), function(f) strsplit(f, "_")[[1]][1], character(1))
        terra::set.names(r, shortName)

        return(r)
      }) |>
        terra::rast()
      terra::set.names(rs, climVars)

      return(rs)
    })
    names(climStacks_msy) <- namesPerYear
  } else {
    climStacks_msy <- list()
  }

  return(climStacks_msy)
}

#' @template ClimateNA_period
#'
#' @export
#' @importFrom terra rast set.names
#' @rdname climateStacksByYear
climateStacksByPeriod <- function(tifs, climVars, type, period) {
  if (length(tifs) > 0) {
    namesPerPeriod <- paste0(type, "_", period)
    climStacks_nrm <- lapply(namesPerPeriod, function(p) {
      rs <- lapply(tifs[[p]], function(files) {
        r <- terra::rast(files)
        shortName <- vapply(basename(files), function(f) strsplit(f, "_")[[1]][1], character(1))
        terra::set.names(r, shortName)

        return(r)
      }) |>
        terra::rast()
      terra::set.names(rs, climVars)

      return(rs)
    })
    names(climStacks_nrm) <- namesPerPeriod
  } else {
    climStacks_nrm <- list()
  }

  return(climStacks_nrm)
}
