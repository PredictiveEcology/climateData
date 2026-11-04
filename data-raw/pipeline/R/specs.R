## Release settings -> batch specs, and loading the package checkout in worker processes.

## Workers are separate R processes and climateData may not be installed: load the checkout
## once per process, so the package's internal pipeline functions are available.
pipeline_load <- function(pkgDir) {
  if (!"climateData" %in% loadedNamespaces()) {
    devtools::load_all(pkgDir, quiet = TRUE)
  }
  invisible(TRUE)
}

## Tiles with a DEM tile, optionally limited to a subset (for testing).
pipeline_tiles <- function(demDir, subset = NULL) {
  tiles <- .demTiles(demDir)
  if (!is.null(subset)) {
    tiles <- intersect(tiles, as.integer(subset))
  }
  tiles
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
