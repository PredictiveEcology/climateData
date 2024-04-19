utils::globalVariables(c(
	":=", "mdc_0", "mdc_m"
))

#' Determine the type of climate variables
#'
#' @template stacks_layers
#'
#' @return character string indicating the type (one of "historic" or "future")
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
#' - `calcATA()` calculates Annual Temperature Anomaly (ATA) from `MAT` and `MAT_normal`;
#' - `calcCMInormal()` calculates mean CMI over multiple normals periods;
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

  checkCalcStackLayers(stks, layers)

  newStk <- lapply(stks, function(x) {
      x[[gsub(paste0(type, "_"), "", layers)]]
    }) |>
      rast()
  set.names(newStk, paste0(gsub(paste0(type, "_"), "", layers), "_", names(newStk))) ## years or period

  return(newStk)
}

#' @export
#' @importFrom terra app subset
#' @rdname calcVars
calcATA <- function(stacks, layers, .dots = NULL) {
  type <- calcStackLayersType(stacks, layers)
  type_years <- grep(paste0("(", paste0(type, collapse = "|"), ")_years"), names(.dots), value = TRUE)
  type_periods <- grep(paste0("(", paste0(type, collapse = "|"), ")_period"), names(.dots), value = TRUE)
  stack_years <- stacks[paste0(gsub("_years", "", type_years), "_", .dots[[type_years]])]
  stack_periods <- stacks[paste0(gsub("_period", "", type_periods), "_", .dots[[type_periods]])]
  stack_periods <- lapply(stack_periods, terra::subset, subset = "MAT_normal")
  checkCalcStackLayers(append(stack_years, stack_periods), layers)
  MAT_norm_mean <- rast(stack_periods) |> terra::app(fun = mean, na.rm = TRUE)

  ATAstack <- lapply(stack_years,  function(x) {
      ata <- x[["MAT"]] - MAT_norm_mean
      set.names(ata, "ATA")
      return(ata)
    }) |>
    rast()
  set.names(ATAstack, paste0("ATA_", names(stack_years))) ## years

  return(ATAstack)
}

#' @export
#' @importFrom terra app set.names
#' @importFrom utils head
#' @rdname calcVars
calcCMInormal <- function(stacks, layers, .dots = NULL) {
  type <- calcStackLayersType(stacks, layers)
  type_years <- grep(paste0("(", paste0(type, collapse = "|"), ")_years"), names(.dots), value = TRUE)
  type_periods <- grep(paste0("(", paste0(type, collapse = "|"), ")_period"), names(.dots), value = TRUE)
  stack_years <- stacks[paste0(gsub("_years", "", type_years), "_", .dots[[type_years]])]
  stack_periods <- stacks[paste0(gsub("_period", "", type_periods), "_", .dots[[type_periods]])]
  checkCalcStackLayers(append(stack_years, stack_periods), layers)

  CMI_norm_mean <- rast(stack_periods) |> terra::app(fun = mean, na.rm = TRUE)

  paste0("CMI_normal_", substr(head(.dots[[type_periods]], 1), 1, 4), "-",
         substr(head(rev(.dots[[type_periods]]), 1), 6, 9)) |>
    set.names(CMI_norm_mean, value = _)

  return(CMI_norm_mean)
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
