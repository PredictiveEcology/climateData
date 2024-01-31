utils::globalVariables(c(
  "gid", "tileid", "year"
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
#' @importFrom DBI dbDisconnect
#' @importFrom dplyr collect filter
getClimateURLs <- function(type = NULL, tile = NULL, years = NULL, msy = NULL,
                           gcm = NULL, ssp = NULL) {
  stopifnot(
    type %in% .allowedClimateTypes,
    !is.null(tile),
    !is.null(years),
    !is.null(msy)
  )

  dbdf <- ClimateNA_sql(ClimateNA_tiles_sqlite(), type)
  climate_db <- dbdf[["db"]]
  climate_df <- dbdf[["df"]]
  rm(dbdf)
  on.exit(DBI::dbDisconnect(climate_db), add = TRUE)

  climate_dt <- if (type == "historic") {
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy, year %in% !!years)
  } else if (type == "future") {
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy, year %in% !!years,
                  gcm == !!gcm, ssp == !!(as.character(ssp)))
  } else if (type == "historic_normals") {
    ## all periods in single zip
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy)
  } else if (type == "future_normals") {
    ## all periods in single zip per gcm_ssp
    dplyr::filter(climate_df, tileid %in% !!tile, msy %in% !!msy,
                  gcm == !!gcm, ssp == !!(as.character(ssp)))
  }
  climate_dt <- dplyr::collect(climate_dt) |>
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
#' @param climateURLs TODO
#'
#' @param climatePath TODO
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
        )
      })
      names(preProcessOut) <- paste0("tile_", climateTile)

      return(preProcessOut)
    }
  ) |>
    invisible()
}

#' Build climate mosaic rasters from ClimateNA tiles
#'
#' @template ClimateNA_tile
#'
#' @param climVars character. climate variables to use to construct mosaics.
#'
#' @template ClimateNA_years
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
#' @importFrom parallel parLapply
#' @importFrom parallelly availableCores
#' @importFrom reproducible checkPath
#' @importFrom sf gdal_utils
#' @importFrom tools file_path_sans_ext
buildClimateMosaics <- function(tile, climVars, years, srcdir = NULL, dstdir = NULL, cl = NULL) {
  if (any(is.null(srcdir), is.null(dstdir))) {
    stop("both 'srcdir' and 'dstdir' must specified and non-NULL.")
  }

  srcdir <- checkPath(srcdir, create = TRUE)
  dstdir <- checkPath(dstdir, create = TRUE)

  cores <- min(length(years), parallelly::availableCores())

  if (is.null(cl)) {
    cl <- parallelly::makeClusterPSOCK(cores,
                                       default_packages = c("fs", "sf", "terra"),
                                       rscript_libs = .libPaths(),
                                       autoStop = TRUE)
  }

  parallel::clusterExport(cl, c("climVars", "dstdir", "srcdir", "tile"), envir = environment())

  ## TODO: switch the loops so outputs a list of annual climate vars
  tifs <- parallel::parLapply(cl, years, function(y) {
    lapply(climVars, function(v) {
      srcfiles <- fs::dir_ls(srcdir, regexp = paste0(tile, "$", collapse = "|"), type = "directory") |>
        fs::dir_ls(regexp = y, type = "directory") |>
        fs::dir_ls(regexp = paste0(v, ".*[.]asc$"), type = "file") ## TODO: improve regex; e.g., bFFP, eFFP vs FFP

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

  names(tifs) <- years

  return(tifs)
}
