utils::globalVariables(c(
	':=', 'mdc_0', 'mdc_m'
))

#' Function to create raster stack for of Monthly Drought Code (MDC)
#'
#'
#' @param inputPath the file path of the directory containing climate files
#'
#' @param years years to use. i.e. c(2011:2100).
#'
#'
#' @param droughtMonths Numeric. Months to calculate Monthly Drought Code (MDC). Must be 4:9
#'
#' @return A list of all years, with each year being the local path for the raster stack that
#'         contains all variables.
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table data.table
#' @importFrom raster crs crs<- getValues ncell raster setValues stack
#' @importFrom sp CRS
#' @importFrom stats na.omit
#'
#' @rdname makeMDC
makeMDC <- function(inputPath, years = NULL, droughtMonths = 4:9) {
	# 1. Make sure it has all defaults
	if (!all(droughtMonths %in% 4:9)) {
		stop("Drought calculation for Months other than April to June is not yet supported") # TODO
	}
	
	variables <- c(paste0("Tmax0", droughtMonths), paste0("PPT0", droughtMonths))
	
	# 2. Check if we have the years chosen (we should lapply through years)
	AllClimateRasters <- lapply(variables, FUN = function(y, Path = inputPath) {
		relevantRasters <- list.files(path = Path, recursive = TRUE, pattern = paste0("*", y), full.names = TRUE)
	})
	AllClimateRasters <- as.list(sort(unlist(AllClimateRasters)))
	MDCrasters <- lapply(years, FUN  = function(year, rasters = AllClimateRasters) {
		toReturn <- grep(pattern = paste0("*", year), x = rasters, value = TRUE)
	})
	
	MDCstacks <- lapply(MDCrasters, FUN = raster::stack)
	MDCstacks <- lapply(MDCstacks, FUN = function(x){
		crs(x) <- sp::CRS(paste0('+proj=longlat +datum=WGS84'))
		return(x)
	})
	
	#set values to actual degrees and not tenths
	MDCstacks <- lapply(MDCstacks, FUN = function(x) {
		tempRasters <- grep(names(x), pattern = "*Tmax")
		ppRasters <- grep(names(x), pattern = '*PP')
		temp <- x[[names(x)[tempRasters]]]
		PPT <- x[[names(x)[ppRasters]]]
		temp <- lapply(names(temp), function(Raster, Stack = temp) {
			Raster <- Stack[[Raster]]
			Raster <- setValues(Raster, as.numeric(getValues(Raster) / 10))
		})
		temp <- stack(temp)
		annualMDCvars <- raster::stack(temp, PPT)
		return(annualMDCvars)
	})
	#Now we have a list of the corrected variables	
	
	# Day length adjustment L_f in Drought Code (taken from Van Wagner 1987)
	L_f <- function(Month) {
		c('4' = 0.9,
			'5' = 3.8,
			'6' = 5.8,
			'7' = 6.4,
			'8' = 5.0,
			'9' = 2.4)[[as.character(Month)]]
		## TODO: [ FIX ] Update for all Months, check latitude problem. Ideally, bring original table in here.
	}
	
	nDays <- function(Month) {
		c('4' = 30,
			'5' = 31,
			'6' = 30,
			'7' = 31,
			'8' = 31,
			'9' = 30)[[as.character(Month)]]
	}
	
	rm(MDCrasters, AllClimateRasters)
	annualMDC <- lapply(MDCstacks, FUN = function(x) {
		months <- 4:9
		mdc <- lapply(months, FUN = function(num, MDCstack = x) {
			ppt <- MDCstack[[paste0("PPT", '0', num)]]
			tmax <- MDCstack[[paste0("Tmax", '0', num)]]
			dt <- data.table(ppt = getValues(ppt), tmax = getValues(tmax), pixID = 1:ncell(tmax))
			dt <- na.omit(dt)
			dt[, mdc_0 := 0]
			dt[, mdc_m := as.integer(round(pmax(mdc_0 + 0.25 * nDays(num) * (0.36 * tmax + L_f(num)) - 
																						400 * log(1 + 3.937 * 0.83 * ppt/(800 * exp(-mdc_0/400))) + 
																						0.25 * nDays(num) * (0.36 * tmax + L_f(num)), 0)))]
			mdc <- setValues(tmax, NA)
			mdc[dt$pixID] <- dt$mdc_m 
			return(mdc)
		})
		mdc <- raster::stack(mdc)
		mdcAnnual <- raster::calc(x = mdc, fun = max)
		return(mdcAnnual)
	})
	names(annualMDC) <- paste0("mdc", years)
	annualMDC <- stack(annualMDC)
}
