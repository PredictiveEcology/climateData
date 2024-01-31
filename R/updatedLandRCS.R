utils::globalVariables(c("."))

#' Create 1951-2010 normals as arithmetic mean of 1951-1980 and 1981-2010 normals
#'
#' Calculates `CMI` as `MAP - Eref`.
#'
#' @param pathToNormalRasters file path of the directory containing climate normal data,
#'                            assumed 1951-1980 and 1981-2010
#'
#' @param rasterToMatch an optional template raster
#'
#' @return a `SpatRaster` of 1951-2010 normal CMI and MAT
#'
#' @author Ian Eddy
#' @export
#' @importFrom reproducible Cache postProcess
#' @importFrom terra crs values mean rast setValues
#' @rdname makeLandRCSnormals_1950_2010_normals
makeLandRCS_1950_2010_normals <- function(pathToNormalRasters, rasterToMatch = NULL) {
  normalMAPs <- list.files(path = pathToNormalRasters, pattern = "MAP[.]asc$",
													 recursive = TRUE, full.names = TRUE)
	stopifnot(grepl("1951_1980", basename(dirname(normalMAPs[1]))) &
							grepl("1981_2010", basename(dirname(normalMAPs[2]))))

	normalMAPs <- rast(normalMAPs)
	crs(normalMAPs) <- .lonlat

	normalMAPVals <- mean(normalMAPs)

	normalErefs <- list.files(path = pathToNormalRasters, pattern = "Eref[.]asc$",
														recursive = TRUE, full.names = TRUE) |>
		rast()
	normalErefVals <- mean(normalErefs)

	## Make CMI -- use raster to avoid file based raster
	normalCMI <- rast(normalMAPs[[1]])
	normalCMI <- setValues(normalCMI, values(normalMAPVals - normalErefVals, mat = FALSE))

	normalMATs <- list.files(path = pathToNormalRasters, pattern = "MAT[.]asc$",
													 recursive = TRUE, full.names = TRUE) |>
		rast()
	normalMATvals <- mean(normalMATs)

	normalMAT <- rast(normalMATs[[1]])
	normalMAT <- setValues(normalMAT, values(normalMATvals, mat = FALSE))

	normals1950_2010 <- c(normalCMI, normalMAT)
	if (!is.null(rasterToMatch) && !is.function(rasterToMatch)) {
		normals1950_2010 <- Cache(postProcess,
															normals1950_2010,
															to = rasterToMatch,
															filename2 = NULL,
															method = "bilinear")
	}

	names(normals1950_2010) <- c("CMInormal", "MATnormal")

	return(normals1950_2010)
}

#' Create projected CMI and ATA - the annual temperature anomaly
#'
#' Calculates `CMI` as `MAP - Eref`
#'
#' @param normalMAT a raster representing normal MAT
#'
#' @param pathToFutureRasters directory of (annual) projected climate layers
#'
#' @param years the projection years (e.g. 2011)
#'
#' @template studyArea
#'
#' @return a list of projected rasters - CMI and ATA
#'
#' @author Ian Eddy
#' @export
#' @importFrom reproducible postProcessTerra
#' @importFrom terra crs<- rast values
#' @rdname makeLandRCS_projectedCMIandATA
makeLandRCS_projectedCMIandATA <- function(normalMAT, pathToFutureRasters, years = 2011:2100,
                                           studyArea = NULL) {

  cmi <- Cache(makeLandRCS_projectedCMI(normalMAT = normalMAT,
                                        pathToFutureRasters = pathToFutureRasters,
                                        years = years, studyArea = studyArea))
  ata <- Cache(makeLandRCS_projectedATA(normalMAT = normalMAT,
                                        pathToFutureRasters = pathToFutureRasters,
                                        years = years, studyArea = studyArea))

  return(list("projectedCMI" = cmi,
              "projectedATA" = ata))
}

#' @export
#' @importFrom reproducible postProcessTo
#' @rdname makeLandRCS_projectedCMIandATA
makeLandRCS_projectedCMI <- function(normalMAT, pathToFutureRasters, years = 2011:2100,
                                           studyArea = NULL) {

  ## MAP
  ppRasters <- list.files(pathToFutureRasters, pattern = "MAP[.]asc$",
                          recursive = TRUE, full.names = TRUE)
  ids <- grep(paste0("_", years, "Y$", collapse = "|"), basename(dirname(ppRasters)))
  ppRasters <- ppRasters[ids]
  stopifnot(length(ppRasters) == length(years))

  ppRasters <- rast(lapply(ppRasters, rast))

  ## Eref
  ErefRasters <- list.files(pathToFutureRasters, pattern = "Eref[.]asc$",
                            recursive = TRUE, full.names = TRUE)
  ids <- grep(paste0("_", years, "Y$", collapse = "|"), basename(dirname(ErefRasters)))
  ErefRasters <- ErefRasters[ids]
  stopifnot(length(ErefRasters) == length(years))

  ErefRasters <- rast(lapply(ErefRasters, rast))

  ## CMI
  CMIstack <- ppRasters - ErefRasters
  names(CMIstack) <- paste0("CMI", years)

  if (!same.crs(CMIstack, normalMAT)) {
    CMIstack <- postProcessTo(CMIstack,
                              to = normalMAT,
                              maskTo = if (is.null(studyArea)) normalMAT else studyArea,
                              method = "bilinear")
  }

  CMIstack
}

#' @export
#' @importFrom reproducible postProcessTo
#' @rdname makeLandRCS_projectedCMIandATA
makeLandRCS_projectedATA <- function(normalMAT, pathToFutureRasters, years = 2011:2100,
                                           studyArea = NULL) {
  MATrasters <- list.files(pathToFutureRasters, pattern = "MAT[.]asc$",
                           recursive = TRUE, full.names = TRUE)
  ids <- grep(paste0("_", years, "Y$", collapse = "|"), basename(dirname(MATrasters)))
  MATrasters <- MATrasters[ids]
  stopifnot(length(MATrasters) == length(years))

  MATrasters <- rast(lapply(MATrasters, rast))
  names(MATrasters) <- paste0("MAT", years)

  # opt <- options("reproducible.gdalwarp" = FALSE)
  # on.exit(options(opt))
  if (!compareGeom(normalMAT, MATrasters, stopOnError = FALSE)) {
    ## this takes a while...
    MATrasters <- postProcessTo(
      MATrasters, # returns SpatRaster file, so arithmetic is faster below
      to = normalMAT,
      maskTo = if (is.null(studyArea)) normalMAT else studyArea,
      method = "bilinear")
  }

  ATAstack <- MATrasters - normalMAT
  names(ATAstack) <- paste0("ATA", years)

  ATAstack
}
