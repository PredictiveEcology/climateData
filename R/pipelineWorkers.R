#' How many pipeline workers a node may run
#'
#' The data pipeline uses at most half of a node's cores. Each ClimateNA run is limited to one
#' CPU (`--cpus 1`), so its CPU use is the number of workers plus `reserved` (on the controlling
#' node: the upload workers and the main process).
#'
#' @param requested integer. Workers asked for.
#' @param cores integer. The node's cores.
#' @param reserved integer. Cores already in use by the pipeline on that node.
#' @param where character. The node, for the message.
#'
#' @return integer: `requested`, or the cap when it is lower (with a message), and at least 1.
#'
#' @keywords internal
.cappedWorkers <- function(requested, cores, reserved, where = "this node") {
  cap <- max(1L, as.integer(floor(cores / 2) - reserved))
  if (requested > cap) {
    message(
      "Using ",
      cap,
      " workers on ",
      where,
      ", not ",
      requested,
      ": the pipeline uses at most half of its ",
      cores,
      " cores (",
      reserved,
      " already in use)."
    )
  }
  as.integer(min(requested, cap))
}
