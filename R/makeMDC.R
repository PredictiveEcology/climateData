utils::globalVariables(c(
	":=", "mdc_0", "mdc_m"
))

#' Create raster of Monthly Drought Code (MDC)
#'
#'
#' @param inputPath the file path of the directory containing climate files
#'
#' @param years years to use. i.e. `c(2011:2100)`.
#'
#'
#' @param droughtMonths Numeric. Months to calculate Monthly Drought Code (MDC). Must be 4:9
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
makeMDC <- function(inputPath, years = NULL, droughtMonths = 4:9) {
	stopifnot(dir.exists(inputPath))

	# 1. Make sure it has all defaults
	if (!all(droughtMonths %in% 4:9)) {
		stop("Drought calculation for Months other than April to June is not yet supported") # TODO
	}

	variables <- c(paste0("Tmax0", droughtMonths), paste0("PPT0", droughtMonths))

	# 2. Check if we have the years chosen (we should lapply through years)
	AllClimateRasters <- lapply(variables, FUN = function(y, Path = inputPath) {
		list.files(path = Path, recursive = TRUE, pattern = paste0("*", y), full.names = TRUE)
	})
	AllClimateRasters <- as.list(sort(unlist(AllClimateRasters)))
	ClimateRasters <- grep(paste0("_", years, "M", collapse = "|"), AllClimateRasters, value = TRUE)
	ClimateRasters <- as.list(ClimateRasters)

	if (length(unlist(ClimateRasters)) != length(years) * length(variables)) {
	  patts <- apply(expand.grid(years, variables), 1, function(x) paste(x, collapse = ".*"))
	  filesHave <- Map(pat = patts, function(pat) grep(ClimateRasters, pattern = pat))
	  missingFiles <- patts[!patts %in% names(unlist(filesHave)) ]
	  years2 <- substr(missingFiles, 1, 4)
	  missingFiles <- gsub("^.{6,6}", "", missingFiles)
	  message("The following year x climate variables are missing")
	  print(split(missingFiles, years2)) # can't "message" a list
	  stop(" ... from:\n  ", inputPath)
	}

	MDCrasters <- lapply(years, FUN  = function(year, rasters = ClimateRasters) {
		grep(pattern = paste0("_", year, "M"), x = rasters, value = TRUE)
	})

	MDCstacks <- lapply(MDCrasters, FUN = rast)

	## set values to actual degrees and not tenths
	MDCstacks <- lapply(MDCstacks, FUN = function(x) {
		tempRasters <- grep(names(x), pattern = "*Tmax")
		ppRasters <- grep(names(x), pattern = '*PP')
		temp <- x[[names(x)[tempRasters]]]
		PPT <- x[[names(x)[ppRasters]]]
		temp <- lapply(names(temp), function(Raster, Stack = temp) {
			Raster <- Stack[[Raster]]
			Raster <- setValues(Raster, as.numeric(values(Raster, mat = FALSE) / 10))
		})
		temp <- rast(temp)
		annualMDCvars <- c(temp, PPT)
		return(annualMDCvars) ## list of the corrected variables
	})

	## Day length adjustment L_f in Drought Code (taken from Van Wagner 1987)
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
		c("4" = 30,
			"5" = 31,
			"6" = 30,
			"7" = 31,
			"8" = 31,
			"9" = 30)[[as.character(Month)]]
	}

	rm(MDCrasters, AllClimateRasters)
	annualMDC <- lapply(MDCstacks, FUN = function(x) {
		months <- 4:9
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
