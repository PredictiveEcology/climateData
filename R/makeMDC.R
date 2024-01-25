utils::globalVariables(c(
	":=", "mdc_0", "mdc_m"
))

#' Create raster of Monthly Drought Code (MDC)
#'
#' @template ClimateNA_srcdstdir
#' @template ClimateNA_tile
#' @template ClimateNA_years
#' @template cl
#'
#' @param droughtMonths integer. months to calculate Monthly Drought Code (MDC). Must be `4:9`.
#'
#' @return A list of all years, with each year being the local path for the raster stack that
#'         contains all variables.
#'
#' @author Tati Micheletti, Ian Eddy, Alex Chubaty
#' @export
#' @importFrom data.table data.table
#' @importFrom terra crs crs<- values ncell rast setValues
#' @importFrom stats na.omit
#' @rdname makeMDC
makeMDC <- function(srcdir, dstdir, tile = NULL, years = NULL, droughtMonths = 4:9, cl = NULL) {
	# 1. Make sure it has all defaults
  stopifnot(!missing(srcdir), !missing(dstdir), !is.null(tile), !is.null(years))

	if (!all(droughtMonths %in% 4:9)) {
		stop("Drought calculation for Months other than April to June is not yet supported")
	  ## TODO: we would need to update the day length adjustments etc. below for other months
	}

	climVars <- c("PPT", "Tmax")
	climVarsByMonth <- lapply(climVars, function(cv) sprintf("%s%02d", cv, droughtMonths)) |> unlist()

	# 2. Check if we have the years chosen (we should lapply through years)
	tifs <- buildClimateMosaics(tile, climVars, years, srcdir = srcdir, dstdir = dstdir, cl = cl)
	tifs_monthly <- grep(paste0(climVarsByMonth, collapse = "|"), tifs, value = TRUE)
	tifs_monthly <- as.list(tifs_monthly)

	if (!all(file.exists(tifs))) {
	  ## TODO: is this strictly necessary; wouldn't GDAL buildvrt fail during buildClimateMosaics()?
	  stop("The following mosaic rasters could not be found:\n",
	       paste(basename(tifs[!file.exists(tifs)]), collapse = ", "), ".\n",
	       "Please check that all source data files are present in ", dstdir, ".")
	}

	MDCrasters <- lapply(years, FUN  = function(year, rasters = tifs_monthly) {
		grep(pattern = paste0("_", year, "_"), x = rasters, value = TRUE)
	})

	MDCstacks <- lapply(MDCrasters, function(files) {
	  r <- terra::rast(files)
	  shortName <- vapply(basename(files), function(f) strsplit(f, "_")[[1]][1], character(1))
	  terra::set.names(r, shortName)

	  return(r)
	})

	## ClimateNA v7.41 (August 01, 2023) release note:
	## Climate variables with decimals are no longer converted to integers in raster format.
	# ## set values to actual degrees and not tenths
	# MDCstacks <- lapply(MDCstacks, FUN = function(x) {
	# 	tempRasters <- grep(names(x), pattern = "Tmax")
	# 	pptRasters <- grep(names(x), pattern = "PPT")
	# 	temp <- x[[names(x)[tempRasters]]]
	# 	PPT <- x[[names(x)[pptRasters]]]
	# 	temp <- lapply(names(temp), function(Raster, Stack = temp) {
	# 		Raster <- Stack[[Raster]]
	# 		Raster <- setValues(Raster, as.numeric(values(Raster, mat = FALSE) / 10))
	# 	})
	# 	temp <- rast(temp)
	# 	annualMDCvars <- c(temp, PPT)
	# 	return(annualMDCvars) ## list of the corrected variables
	# })

	## Day length adjustment L_f in Drought Code (taken from Van Wagner 1987)
	L_f <- function(Month) {
		c("4" = 0.9,
			"5" = 3.8,
			"6" = 5.8,
			"7" = 6.4,
			"8" = 5.0,
			"9" = 2.4)[[as.character(Month)]]
		## TODO: [ FIX ] Update for all Months, check latitude problem. Ideally, bring original table in here.
	}

	nDays <- function(Month) {
		c("4" = 30,
			"5" = 31,
			"6" = 30,
			"7" = 31,
			"8" = 31,
			"9" = 30)[[as.character(Month)]]
	}

	annualMDC <- lapply(MDCstacks, FUN = function(x) {
		months <- droughtMonths
		mdc <- lapply(months, FUN = function(num, MDCstack = x) {
			ppt <- MDCstack[[paste0("PPT", "0", num)]]
			tmax <- MDCstack[[paste0("Tmax", "0", num)]]
			dt <- data.table(ppt = values(ppt, mat = FALSE),
			                 tmax = values(tmax, mat = FALSE),
			                 pixID = 1:ncell(tmax))
			dt <- na.omit(dt)
			dt[, mdc_0 := 0]
			## adjusted MDC calculation from:
			## Bergeron, Y., Cyr, D., Girardin, M.P. and Carcaillet, C., 2010. Will climate change drive
			## 21st century burn rates in Canadian boreal forest outside of its natural variability:
			## collating global climate model experiments with sedimentary charcoal data. International
			## Journal of Wildland Fire, 19(8), pp.1127-1139.
			dt[, mdc_m := as.integer(round(pmax(mdc_0 + 0.25 * nDays(num) * (0.36 * tmax + L_f(num)) -
																						400 * log(1 + 3.937 * 0.83 * ppt/(800 * exp(-mdc_0/400))) +
																						0.25 * nDays(num) * (0.36 * tmax + L_f(num)), 0)))]
			suppressWarnings({
				mdc <- setValues(tmax, NA)
			}) ## TODO: why is min/max raster values triggering -Inf/Inf ??
			mdc[dt$pixID] <- dt$mdc_m
			return(mdc)
		})
		mdc <- rast(mdc)
		mdcAnnual <- max(mdc)
		return(mdcAnnual)
	})
	names(annualMDC) <- paste0("mdc", years)
	annualMDC <- rast(annualMDC)

	return(annualMDC)
}
