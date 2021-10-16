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
#' @return a raster stack of 1951-2010 normal CMI and MAT
#'
#' @author Ian Eddy
#' @export
#' @importFrom raster crs getValues mean raster setValues stack 
#' @importFrom magrittr %>%
#' @importFrom reproducible Cache postProcess
#' @rdname makeLandRCSnormals_1950_2010_normals
makeLandRCS_1950_2010_normals <- function(pathToNormalRasters, rasterToMatch = NULL) {
	normalMAPs <- list.files(path = pathToNormalRasters, pattern = "MAP[.]asc$",
													 recursive = TRUE, full.names = TRUE)
	stopifnot(grepl("1951_1980", basename(dirname(normalMAPs[1]))) &
							grepl("1981_2010", basename(dirname(normalMAPs[2]))))
	
	lonlat <- "+init=epsg:4326 +proj=longlat"
	
	normalMAPs <- stack(normalMAPs)
	crs(normalMAPs) <- lonlat
	
	normalMAPVals <- mean(normalMAPs)
	
	normalErefs <- list.files(path = pathToNormalRasters, pattern = "Eref[.]asc$",
														recursive = TRUE, full.names = TRUE) %>%
		stack()
	crs(normalErefs) <- lonlat
	normalErefVals <- mean(normalErefs)
	
	## Make CMI -- use raster to avoid file based raster
	normalCMI <- raster(normalMAPs[[1]])
	normalCMI <- setValues(normalCMI, getValues(normalMAPVals - normalErefVals))
	
	normalMATs <- list.files(path = pathToNormalRasters, pattern = "MAT[.]asc$",
													 recursive = TRUE, full.names = TRUE) %>%
		stack()
	crs(normalMATs) <- lonlat
	normalMATvals <- mean(normalMATs)
	
	normalMAT <- raster(normalMATs[[1]])
	normalMAT <- setValues(normalMAT, getValues(normalMATvals))
	
	normals1950_2010 <- stack(normalCMI, normalMAT)
	if (!is.null(rasterToMatch)) {
		normals1950_2010 <- Cache(postProcess,
															normals1950_2010,
															rasterToMatch = rasterToMatch, 
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
#' @param pathToFutureRasters directory of (annual) projected climate layers
#' @param years the projection years (e.g. 2011)
#' 
#' @return a list of projected raster stacks - CMI and ATA
#'
#' @author Ian Eddy
#' @export
#' @importFrom raster crs getValues raster setValues compareCRS 
#' @importFrom reproducible postProcess
#' @rdname makeLandRCS_projectedCMIandATA
makeLandRCS_projectedCMIandATA <- function(normalMAT, pathToFutureRasters, years = 2011:2100) {
	MATrasters <- list.files(pathToFutureRasters, pattern = "MAT[.]asc$",
													 recursive = TRUE, full.names = TRUE)
	MATrasters <- stack(MATrasters)
	crs(MATrasters) <- "+init=epsg:4326 +proj=longlat"
	
	names(MATrasters) <- paste0("MAT", years)
	MATrasters <- stack(MATrasters)
	
	if (!compareCRS(normalMAT, MATrasters)) {
		MATrasters <- postProcess(MATrasters, rasterToMatch = normalMAT)
	}
	
	ATAstack <- MATrasters - normalMAT
	names(ATAstack) <- paste0("ATA", years)

	#MAP
	ppRasters <- list.files(pathToFutureRasters, pattern = "MAP[.]asc$",
													recursive = TRUE, full.names = TRUE)
	ppRasters <- stack(ppRasters)
	crs(ppRasters) <- "+init=epsg:4326 +proj=longlat"
	
	#Eref
	ErefRasters <- list.files(pathToFutureRasters, pattern = "Eref[.]asc$",
														recursive = TRUE, full.names = TRUE)
	ErefRasters <- stack(ErefRasters)
	crs(ErefRasters) <- "+init=epsg:4326 +proj=longlat"
	
	#Calc CMI
	CMIstack <- ppRasters - ErefRasters
	names(CMIstack) <- paste0("CMI", years)
	
	if (!compareCRS(CMIstack, normalMAT)) {
		CMIstack <- postProcess(CMIstack, 
														rasterToMatch = normalMAT, 
														method = 'bilinear')
	}
	
	return(list(
		"projectedCMI" = CMIstack,
		"projectedATA" = ATAstack
	))
}
