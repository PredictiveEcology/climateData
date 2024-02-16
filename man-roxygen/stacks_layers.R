#' @param stacks named list of `SpatRaster` objects.
#'               names should correspond to `<type>_<year>` or `<type>_<period>`,
#'               where `type` is one of "historic" or "future"; and `year` or `period`
#'               correspond to a climate year or climate period, respectively.
#'
#' @param layers layer names (matching ClimateNA variable names) to check for in
#'               each element of `stacks`.
