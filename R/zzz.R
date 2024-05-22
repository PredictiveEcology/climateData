.parallelBackends <- c("parallel", "future")

stopForInvalidBackend <- function() {
  stop(sprintf("Invalid 'climateData.parallel.backend': '%s'.\nMust be one of: '%s'.",
               getOption("climateData.parallel.backend"),
               paste(.parallelBackends, collapse = "', '")))
}

.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.climateData <- list(
    climateData.parallel.backend = "parallel" ## "future" also supported
  )
  toset <- !(names(opts.climateData) %in% names(opts))
  if (any(toset)) options(opts.climateData[toset])
}
