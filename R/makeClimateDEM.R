#' Create a DEM for use with ClimateNA
#'
#'
#' @param studyArea a studyArea for cropping the gtopo30 DEM
#' @param bufferArcSec numeric  - arc seconds by which to buffer the studyArea to ensure seamless coverage.
#' Recommended to be at least the same as \code{res}
#' @param arcSecRes the resolution of the output dem in arc seconds
#' @param DEMdestinationPath DESCRIPTION NEEDED
#' @param destinationPath the directory to save the output raster
#' @param filename2 the name of the output DEM without the .ASC suffix
#' 
#' @return a DEM in ASCII format
#'
#' @author Ian Eddy
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom raster buffer compareCRS crop mask projectRaster resample trim writeRaster
#' @importFrom sp spTransform CRS
#' 
#' @rdname makeClimateDEM
makeClimateDEM <- function(studyArea, arcSecRes = c(210, 210), bufferArcSec, DEMdestinationPath,
													 destinationPath, filename2) {
	gtopo30N <- prepInputs(url = 'https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/view?usp=sharing',
												 destinationPath = DEMdestinationPath, 
												 overwrite = TRUE)
	
	if (arcSecRes[1] != arcSecRes[2]) {
		stop('.asc format requires x and y dimensions to be equal. Please adjust arcSecRes')
	}
	
	if (!is.null(bufferArcSec)) {
		studyArea <- spTransform(studyArea, CRSobj = sp::CRS(paste0('+proj=longlat +datum=WGS84')))
		studyArea <- buffer(studyArea, bufferArcSec/60/60)
	}
	
	if (!compareCRS(studyArea, gtopo30N)) {
		studyArea <- spTransform(studyArea, CRSobj = sp::CRS(paste0('+proj=longlat +datum=WGS84')))
	}
	
	
	studyArea <- spTransform(studyArea, CRSobj = crs(gtopo30N))
	gtopo30N <- crop(gtopo30N, studyArea)
	gtopo30N <- mask(gtopo30N, studyArea)
	outputFilename <- paste0(destinationPath, "/", filename2, '.asc')
	gtopo30N <- suppressWarnings(projectRaster(gtopo30N, crs = CRS(paste0('+proj=longlat +datum=WGS84')), res = arcSecRes/60/60))
	#resample doesn't have a res argument - really?  
	gtopo30N <- trim(gtopo30N)
	writeRaster(gtopo30N, filename = paste0(destinationPath, '/', filename2, '.asc'), overwrite = TRUE)
	return(gtopo30N)
	
}