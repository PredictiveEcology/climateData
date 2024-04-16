#' @param cl cluster object. if `NULL` (default), a cluster will be created using up to
#'           `length(climVar)` number of CPU cores for parallel computation.
#'           Users can pass their own `cl` object or specify option
#'           `parallelly.availableCores.fallback` to reduce the number of cores used.
#'           Users may also need to specify setting up the cluster using 'sequential',
#'           rather than the default 'parallel', via `parallelly.makeNodePSOCK.setup_strategy`.
#'           See `?parallelly.options`.
