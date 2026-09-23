utils::globalVariables(c(
	":=", "mdc_0", "mdc_m"
))

#' Determine the type of climate variables
#'
#' @template stacks_layers
#'
#' @return character string indicating the type (one of "historical" or "future")
#'
#' @export
#' @importFrom dplyr first
calcStackLayersType <- function(stacks, layers) {
  stopifnot(!missing(stacks), !missing(layers))

  type <- vapply(layers, function(lyr) {
    strsplit(lyr, "_")[[1]] |> dplyr::first()
  }, character(1)) |>
    unique()

  return(type)
}

#' Check layer names within a list of raster stacks
#'
#' @template stacks_layers
#'
#' @export
#' @importFrom dplyr first
checkCalcStackLayers <- function(stacks, layers) {
  stopifnot(!missing(stacks), !missing(layers))

  type <- calcStackLayersType(stacks, layers)
  layers <- vapply(layers, function(l) {
    gsub(paste0("(", paste0(type, "_"), ")", collapse = "|"), "", l)
  }, character(1))
  stackLayers <- lapply(stacks, names) |> unlist() |> unique()
  stopifnot(all(layers %in% stackLayers)) ## TODO: too general; specific layers in specific rasters
}

#' Create raster stacks of climate variables
#'
#' Produce a `SpatRaster` object corresponding to a single climate variable,
#' with layers corresponding to climate years or periods.
#'
#' - `calcAsIs()` returns the climate variable without modification;
#' - `calcMDC()` calculates Monthly Drought Code (MDC) from `Tmax` and `PPT` for April-September;
#'
#' @template stacks_layers
#'
#' @template dots
#'
#' @template return_calcVars
#'
#' @author Alex Chubaty, Ian Eddy, Tati Micheletti
#'
#' @export
#' @rdname calcVars
calcAsIs <- function(stacks, layers, .dots = NULL) {
  stopifnot(length(layers) == 1)

  type <- calcStackLayersType(stacks, layers)
  type_years <- grep(paste0("(", paste0(type, collapse = "|"), ")_years"), names(.dots), value = TRUE)
  type_periods <- grep(paste0("(", paste0(type, collapse = "|"), ")_period"), names(.dots), value = TRUE)

  stack_years <- list()
  stack_periods <- list()

  if (length(type_years) > 0) {
    names_years <- paste0(gsub("_years", "", type_years), "_", .dots[[type_years]])
    if (all(names_years %in% names(stacks))) {
      stack_years <- stacks[names_years]
    }
  }

  if (length(type_periods) > 0) {
    names_periods <- paste0(gsub("_period", "", type_periods), "_", .dots[[type_periods]])
    if (all(names_periods %in% names(stacks))) {
      stack_periods <- stacks[names_periods]
    }
   }

  stks <- append(stack_years, stack_periods)
  #names(stks) = "historical_2011" etc - but name of list element is e.g. CMI

  checkCalcStackLayers(stks, layers)

  newStk <- lapply(stks, function(x) {
    rasNames <- names(x)
    possNames <- gsub(paste0(type, "_"), "", layers)
    #if layers is length > 1 (normal periods) we must protect like so
    x[[rasNames %in% possNames]]
    }) |>
      rast()
  #newStk is named "historical_2011", "historical_2023" - gsub returns CMI
  # and it becomes "CMI_historical_2011"
  set.names(newStk, paste0(gsub(paste0(type, "_"), "", layers), "_", names(newStk))) ## years or period

  return(newStk)
}


#' Create raster of Monthly Drought Code (MDC)
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom terra crs crs<- values ncell rast set.names setValues
#' @importFrom stats na.omit
#' @rdname calcVars
calcMDC <- function(stacks, layers, .dots = NULL) {
  type <- calcStackLayersType(stacks, layers)
  stack_years <- stacks[paste0(type, "_", .dots[[paste0(type, "_years")]])]
  checkCalcStackLayers(stack_years, layers)

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

	annualMDC <- lapply(stack_years, FUN = function(x) {
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
	set.names(annualMDC, paste0("MDC_", names(stack_years))) ## years

	return(annualMDC)
}

## Drought Code constants (Van Wagner 1987, as in cffdrs): day-length factors for the northern
## hemisphere (cffdrs `fl01`) and days per month, January to December.
.mdcLf <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5.0, 2.4, 0.4, -1.6, -1.6)
.mdcDays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

## One month of the Drought Code from its value at the start of the month (`dc0`), the month's mean
## daily maximum temperature (`tmax`, C) and total precipitation (`ppt`, mm): half the month's drying,
## then the month's rain, then the other half (Girardin & Wotton 2009). Potential evapotranspiration
## per day is cffdrs's 0.5 * (0.36 * (T + 2.8) + Lf) with T >= -2.8; effective rain is 0.83 * P (the
## daily -1.27 mm per rain event is dropped, as the number of events in a month is unknown).
.mdcStep <- function(dc0, tmax, ppt, m) {
  pe <- pmax(0.5 * (0.36 * (pmax(tmax, -2.8) + 2.8) + .mdcLf[m]), 0)
  half <- dc0 + 0.5 * .mdcDays[m] * pe
  Q <- 800 * exp(-half / 400) + 3.937 * 0.83 * ppt
  pmax(400 * log(800 / Q), 0) + 0.5 * .mdcDays[m] * pe
}

## Spring Drought Code from last autumn's value (`DCf`) and the winter's precipitation (`rw`, mm), with
## carry-over fraction `a` and precipitation effectiveness `b` (cffdrs::overwinter_drought_code(); Lawson
## & Armitage 2008). Never below 15, the standard spring start.
.overwinterDC <- function(DCf, rw, a = 0.75, b = 0.75) {
  Qs <- a * 800 * exp(-DCf / 400) + b * 3.94 * rw
  pmax(400 * log(800 / Qs), 15)
}

#' Create raster of overwintered Monthly Drought Code (oMDC)
#'
#' The Drought Code carried from month to month (April to October) and from year to year: each
#' April starts from the previous October's value, reduced by that winter's precipitation
#' (November to March) as in `cffdrs::overwinter_drought_code()`. `calcMDC()` instead starts every
#' month from 0, so it has no memory of an earlier dry month or a dry autumn. The value for a year is
#' the mean of the May to September mid-month values.
#'
#' Years must be consecutive, because each year starts from the previous one. The first year starts
#' at 15 (a fully wet spring), so the first few years are approximate: supply spin-up years before
#' those you need. [climateLayers()] does this for `"oMDC"`. Layers are returned for every supplied
#' year.
#'
#' Needs monthly `PPT01`-`PPT12` and `Tmax04`-`Tmax10`.
#'
#' @export
#' @importFrom terra setValues values
#' @rdname calcVars
calcOMDC <- function(stacks, layers, .dots = NULL) {
  type <- calcStackLayersType(stacks, layers)
  yrs <- sort(as.integer(.dots[[paste0(type, "_years")]]))
  if (length(yrs) > 1 && any(diff(yrs) != 1L)) {
    stop("calcOMDC() needs consecutive years (each year starts from the previous October); got ",
         paste(yrs, collapse = ", "))
  }
  stack_years <- stacks[paste0(type, "_", yrs)]
  checkCalcStackLayers(stack_years, layers)
  need <- c(sprintf("PPT%02d", 1:12), sprintf("Tmax%02d", 4:10))
  missingVars <- setdiff(need, gsub(paste0("^", type, "_"), "", layers))
  if (length(missingVars)) {
    stop("calcOMDC() needs monthly ", paste(missingVars, collapse = ", "))
  }

  v <- function(x, nm) values(x[[nm]], mat = FALSE)
  octPrev <- NULL
  winterPrev <- NULL
  out <- vector("list", length(stack_years))
  for (i in seq_along(stack_years)) {
    x <- stack_years[[i]]
    ppt <- lapply(1:12, function(m) v(x, sprintf("PPT%02d", m)))
    dc <- if (is.null(octPrev)) rep(15, length(ppt[[1]])) else
      .overwinterDC(octPrev, winterPrev + ppt[[1]] + ppt[[2]] + ppt[[3]])
    sumMid <- 0
    for (m in 4:10) {
      dcEnd <- .mdcStep(dc, v(x, sprintf("Tmax%02d", m)), ppt[[m]], m)
      if (m %in% 5:9) sumMid <- sumMid + (dc + dcEnd) / 2
      dc <- dcEnd
    }
    octPrev <- dc
    winterPrev <- ppt[[11]] + ppt[[12]]
    out[[i]] <- setValues(x[[1]], sumMid / 5)
  }
  out <- rast(out)
  set.names(out, paste0("oMDC_", names(stack_years)))
  out
}
