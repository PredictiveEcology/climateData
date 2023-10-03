#' Create a DEM for use with `ClimateNA`
#'
#' @param studyArea a study area for cropping the `gtopo30` DEM
#' @param bufferArcSec numeric, arcseconds by which to buffer the `studyArea` to
#'                     ensure seamless coverage.
#'                     Recommended to be at least the same as `res`
#' @param arcSecRes the resolution of the output DEM in arcseconds
#' @param DEMdestinationPath DESCRIPTION NEEDED
#' @param destinationPath the directory to save the output raster
#' @param filename2 the name of the output DEM without the .ASC suffix
#'
#' @return a DEM in ASCII format
#'
#' @author Ian Eddy
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom terra buffer compareGeom crop mask project resample same.crs trim vect writeRaster
#' @rdname makeClimateDEM
makeClimateDEM <- function(studyArea, arcSecRes = c(180, 180), bufferArcSec = 180,
                           DEMdestinationPath, destinationPath, filename2) {
	if (Sys.info()["sysname"] != "Windows")
		stop("ClimateNA will only accept .asc files created by Windows.")

  if (!is(studyArea, "SpatVector")) {
    studyArea <- vect(studyArea)
  }

	gtopo30N <- prepInputs(
	  url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/",
	  destinationPath = DEMdestinationPath,
	  overwrite = TRUE
	 )

	if (arcSecRes[1] != arcSecRes[2]) {
		stop(".asc format requires x and y dimensions to be equal. Please adjust arcSecRes.")
	}

	if (!is.null(bufferArcSec)) {
		studyArea <- transform(studyArea, "epsg:4326")
		studyArea <- buffer(studyArea, bufferArcSec/60/60)
	}

	if (!same.crs(studyArea, gtopo30N)) {
		studyArea <- transform(studyArea, "epsg:4326")
	}

	studyArea <- transform(studyArea, crs(gtopo30N))
	gtopo30N <- crop(gtopo30N, studyArea)
	gtopo30N <- mask(gtopo30N, studyArea)
	outputFilename <- file.path(destinationPath, paste0(filename2, ".asc"))
	gtopo30N <- project(gtopo30N, "epsg:4326", res = arcSecRes/60/60)
	#resample doesn't have a res argument - really?
	gtopo30N <- trim(gtopo30N)
	writeRaster(gtopo30N,
	            filename = file.path(destinationPath, paste0(filename2, ".asc")),
	            overwrite = TRUE)
	return(gtopo30N)
}
