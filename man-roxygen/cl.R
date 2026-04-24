#' @param cl Optional cluster object.
#' If option `climateData.parallel.backend` is "parallel" (default), and `cl` is NULL (default),
#' a cluster will be created using up to `length(climVar)` number of CPU cores.
#' When using the "parallel" backend, users can pass their own `cl` object or specify option
#' `parallelly.availableCores.fallback` to control the number of cores used.
#' If `climateData.parallel.backend` is "future", `cl` is ignored, as the user is expected to
#' set up the future plan themselves (see `future::plan`).
#' Users may also need to specify setting up the cluster using 'sequential',
#' rather than the default 'parallel', via `parallelly.makeNodePSOCK.setup_strategy`.
#' See `?parallelly.options`.
