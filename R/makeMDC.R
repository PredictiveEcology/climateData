utils::globalVariables(c(
	":=", "mdc_0", "mdc_m"
))

#' Create raster of Monthly Drought Code (MDC)
#'
#' @template stacks_layers
#'
#' @param ... additional arguments (not used).
#'
#' @return A list of all years, with each year being the local path for the raster stack that
#'         contains all variables.
#'
#' @author Tati Micheletti, Ian Eddy, Alex Chubaty
#' @export
#' @importFrom data.table data.table
#' @importFrom terra crs crs<- values ncell rast set.names setValues
#' @importFrom stats na.omit
#' @rdname makeMDC
makeMDC <- function(stacks, layers, ...) {
	# 1. Make sure it has all defaults
  stopifnot(!missing(stacks), !missing(layers),
            all(layers %in% sapply(stacks, names, simplify = TRUE)))

  droughtMonths <- sapply(layers, function(x) substr(x, nchar(x) - 1, nchar(x))) |>
    as.integer() |>
    unique() |>
    sort()

  if (!all(droughtMonths %in% 4:9)) {
		stop("Drought calculation for Months other than April to June is not yet supported")
	  ## TODO: we would need to update the day length adjustments etc. below for other months
	}

	## Day length adjustment L_f in Drought Code (taken from Van Wagner 1987)
	L_f <- function(Month) {
		c("4" = 0.9,
			"5" = 3.8,
			"6" = 5.8,
			"7" = 6.4,
			"8" = 5.0,
			"9" = 2.4)[[as.character(Month)]]
		## TODO: [ FIX ] Update for all Months, check latitude problem.
	  ##       Ideally, bring original table in here.
	}

	nDays <- function(Month) {
		c("4" = 30,
			"5" = 31,
			"6" = 30,
			"7" = 31,
			"8" = 31,
			"9" = 30)[[as.character(Month)]]
	}

	annualMDC <- lapply(stacks, FUN = function(x) {
	  mdc <- lapply(droughtMonths, FUN = function(mnth, MDCstack = x) {
			ppt <- MDCstack[[sprintf("PPT%02d", mnth)]]
			tmax <- MDCstack[[sprintf("Tmax%02d", mnth)]]

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
			dt[, mdc_m := as.integer(round(pmax(mdc_0 + 0.25 * nDays(mnth) * (0.36 * tmax + L_f(mnth)) -
																						400 * log(1 + 3.937 * 0.83 * ppt / (800 * exp(-mdc_0 / 400))) +
																						0.25 * nDays(mnth) * (0.36 * tmax + L_f(mnth)), 0)))]
			suppressWarnings({
				mdc <- setValues(tmax, NA)
			}) ## TODO: why is min/max raster values triggering -Inf/Inf ??
			mdc[dt$pixID] <- dt$mdc_m

			return(mdc)
		}) |>
	    rast() |>
	    max()

	  set.names(mdc, "MDC")

		return(mdc)
	}) |>
	  rast()
	set.names(annualMDC, paste0("MDC_", names(stacks))) ## years

	return(annualMDC)
}
