#' Helper to set up the required layers for canClimateData
#'
#' A constructor of the list to pass to `canClimateData` SpaDES module, and `prepClimateLayers`
#' function.
#'
#' @param .climVars Character vector. This should be a vector naming the climate layers
#'   of interest, as specified in the Climate NA web page documentation. See:
#'   \url{https://climatena.ca/Help2#_var}
#'
#' @param historical Logical. If `TRUE`, then include historical data in the download.
#'
#' @param projected Logical. If `TRUE`, then include projected data in the download.
#'
#' @param fun A quoted function name, e.g., `quote(calcAsIs)`, which is the default.
#'   No other examples have been used and may not work.
#'
#' @details
#'
#' The canClimateData module requires a particular format to specify which
#' climater layers to download. For example:
#' ```
#' climateVariables = list(
#'   historical_CMDsm = list(
#'     vars = "historical_CMD_sm",
#'     fun = quote(calcAsIs),
#'     .dots = list(historical_years = 1991:2022)
#'   ),
#'   projected_CMDsm = list(
#'     vars = "future_CMD_sm",
#'     fun = quote(calcAsIs),
#'     .dots = list(future_years = 2011:2100)
#'   )
#' )
#' ```
#' The above shows that it must be a list of lists, where the names of the list elements
#' must have one underscore, but the variables of the same climate layers may
#' need two underscores. Similarly, the default function to pass to `prepClimateLayers`
#' is `calcAsIs`, but this is not set by default, so the list must specify this. Finally,
#' the hidden list element, `.dots` ("dot dots") must be present and must be a list
#' of named years for data, and it must use the prefix `future` when it is about
#' climate projections.
#'
#' @return A named list of named lists that can be passed to the `sim` object named
#'   `climateLayers`, which is an input requirement for `canClimateData` SpaDES module.
#'
#' @export
climateLayers <- function(.climVars = "CMD_sm", historical = TRUE, projected = TRUE,
                          fun = quote(calcAsIs)) {
  hps <- c()
  if (isTRUE(historical))
    hps <- c(historical = "historical")
  if (isTRUE(projected))
    hps <- c(hps, future = "projected")

  rr <- Map(hp = unname(hps), nam = names(hps), function(hp, nam) {
    Map(cv = .climVars, function(cv) {
      ll <- list(vars = paste0(nam, "_", cv),
                 fun = fun)
      .dots = if (nam == "historical")
        list(1991:2022)
      else
        list(2011:2100)
      ll <- append(ll, list(.dots = .dots |> stats::setNames(paste0(nam, "_years"))))
    })
  }) |> unlist(recursive = FALSE)

  names(rr) <- gsub("_", "", names(rr)) # removes _ in e.g., CMD_sm
  names(rr) <- gsub("\\.", "_", names(rr)) # converts remaining . to a _ in e.g., projected_

  rr
}
