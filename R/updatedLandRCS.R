utils::globalVariables(c("."))


#' Create 1950-2010 normals as arithmetic mean of 1950-1980 and 1980-2010 normals.
#' Calculates CMI as MAP - Eref
#' @param pathToNormalRasters file path of the directory containing climate normal data, assumed 1950-1980 and 1980-2010
#' @param rasterToMatch an optional template raster
#' @return a raster stack of 1950-2010 normal CMI and MAT
#'
#'
#' @author Ian Eddy
#' @export
#' @importFrom raster crs getValues raster setValues stack 
#' @importFrom magrittr %>%
#' @importFrom reproducible postProcess
#' @rdname makeLandRCSnormals_1950_2010_normals
makeLandRCS_1950_2010_normals <- function(pathToNormalRasters, rasterToMatch = NULL) {
	
	#Normal CMI	
	#normal MAP
	normalMAPs <- list.files(path = pathToNormalRasters, pattern = "MAP[.]asc$",
													 recursive = TRUE, full.names = TRUE)
	normalMAPs <- stack(normalMAPs)
	crs(normalMAPs) <- "+init=epsg:4326 +proj=longlat"
	normalMAPVals <- (getValues(normalMAPs[[1]]) + getValues(normalMAPs[[2]]))/2
	
	#normal Eref
	normalErefs <- list.files(path = pathToNormalRasters, pattern = "Eref[.]asc$",
														recursive = TRUE, full.names = TRUE) %>%
		lapply(., FUN = raster) %>%
		stack(.)
	crs(normalErefs) <- "+init=epsg:4326 +proj=longlat"
	normalErefVals <- (getValues(normalErefs[[1]]) + getValues(normalErefs[[2]]))/2
	
	#Make CMI
	#use raster to avoid FBR
	normalCMI <- setValues(raster(normalMAPs[[1]]), normalMAPVals - normalErefVals)
	
	normalMATs <- list.files(path = pathToNormalRasters, pattern = "MAT[.]asc$",
													 recursive = TRUE, full.names = TRUE)
	normalMATs <- stack(normalMATs)
	normalMATvals <- (getValues(normalMATs[[1]]) + getValues(normalMATs[[2]]))/2
	
	normalMAT <- setValues(raster(normalMATs[[1]]), normalMATvals)
	
	normals1950_2010 <- stack(normalCMI, normalMAT)
	if (!is.null(rasterToMatch)) {
		normals1950_2010 <- postProcess(normals1950_2010, 
																		rasterToMatch = rasterToMatch, 
																		filename2 = NULL, 
																		method = "bilinear")
	}
	
	names(normals1950_2010) <- c("CMInormal", "MATnormal")
	return(normals1950_2010)
}
	
#' Create projected CMI and ATA - the annnual temperature anomaly
#' Calculates CMI as MAP - Eref
#' @param normalMAT a raster representing normal MAT
#' @param pathToFutureRasters directory of projected climate layers
#' @param years the projection years (e.g. 2011)
#' @return a list of projected raster stacks - CMI and ATA
#'
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
	
	# ATAstack <- stack(ATArasters)
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
	
	if (!compareCRS(CMIstack, normalMAT)){
		CMIstack <- postProcess(CMIstack, 
														rasterToMatch = normalMAT, 
														method = 'bilinear')
	}
	
	return(list(
		"projectedCMI" = CMIstack,
		"projectedATA" = ATAstack
	))
	
}
