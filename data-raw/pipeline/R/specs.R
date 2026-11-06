## Release settings -> batch specs, and loading the package checkout in worker processes.

## Workers are separate R processes and climateData may not be installed: load the checkout
## once per process, so the package's internal pipeline functions are available. Workers start
## in the package root (local workers inherit the main process's directory; crew.ssh workers cd
## to `projdir`), so the checkout's path is never a target dependency: moving the checkout
## rebuilds nothing.
pipeline_load <- function() {
  if (!"climateData" %in% loadedNamespaces()) {
    devtools::load_all(".", quiet = TRUE)
  }
  invisible(TRUE)
}

## Tiles with a DEM tile, optionally limited to a subset (for testing); stops if there are none,
## or if a subset tile has no DEM.
pipeline_tiles <- function(demDir, subset = NULL) {
  .demTiles(demDir, subset)
}

## The batch specs for one archive type, from the release settings.
pipeline_specs <- function(type, tiles, cfg) {
  span <- function(x) seq(as.integer(x[[1]]), as.integer(x[[2]]))
  switch(
    type,
    historical = .batchSpecs("historical", tiles, years = span(cfg$historical_years)),
    future = .batchSpecs(
      "future",
      tiles,
      gcms = unlist(cfg$gcms),
      ssps = unlist(cfg$ssps),
      years = span(cfg$future_years)
    ),
    historical_normals = .batchSpecs(
      "historical_normals",
      tiles,
      periods = unlist(cfg$historical_periods)
    ),
    future_normals = .batchSpecs(
      "future_normals",
      tiles,
      gcms = unlist(cfg$gcms),
      ssps = unlist(cfg$ssps),
      periods = unlist(cfg$future_periods)
    )
  )
}
