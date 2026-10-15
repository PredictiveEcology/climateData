#' Check that a run's times are calendar years the climate data can serve
#'
#' Climate data are by calendar year, from the first historical year to the last projected year
#' that [available()] reports (1901 to 2100). A run whose start is outside that span, such as
#' the model years 0-10 that `SpaDES.project::setupProject()` gives when `times` is not set,
#' cannot be matched to any climate year. A run may end after the last year.
#'
#' @param start,end Start and end times of the simulation.
#' @param years Optional years the caller will load (e.g. a module's climate-year parameters); the span is then
#'   their range instead of the span [available()] reports.
#' @param module Name of the calling module, for the message.
#' @param hint Optional text added to the end of the message, saying how to proceed.
#'
#' @return `NULL`, invisibly; an error if `start` is outside the span of the climate data.
#'
#' @export
#'
#' @examples
#' checkCalendarYears(2020, 2120, module = "myModule")
checkCalendarYears <- function(start, end, module = "this module", hint = NULL, years = NULL) {
  if (is.null(years)) {
    years <- unlist(lapply(c("historical", "future"), function(type) available(type)[["years"]]))
  }
  span <- range(years)
  if (start < span[1] || start > span[2]) {
    stop(module, ": times are model years ", start, "-", end, "; this module needs calendar years ",
         "(the climate data cover ", span[1], "-", span[2], ", so `start` must be in that range), ",
         "e.g. times = list(start = 2020, end = 2120). ", hint, call. = FALSE)
  }
  invisible(NULL)
}
