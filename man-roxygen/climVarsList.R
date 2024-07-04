#' @param climateVarsList Named list of lists, specifying the climate variables to extract and/or
#' calculate.
#' The name of each outer list element must be prefixed by either `future_` or `historical_`,
#' and each inner list should consist of the following named elements:
#'
#' - `vars`: the raw variables used to derive the target variable;
#' - `fun`: a quoted function used to derive the target variable,
#'          where `quote(calcAsIs)` denotes target variables that ARE the raw variable:
#'  - `.dots`: additional arguments passed to `fun`.
#'
#' See examples.
