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

## Canada, whose cells keep ClimateNA's values where the DEM has none: GADM 4.1, level 0, at its
## highest resolution, as data-raw/02-ClimateNA_tiles.R uses for the tiles. Downloaded once into
## <out_root>/boundary/; returns the file.
pipeline_canada <- function(outRoot) {
  dir <- file.path(outRoot, "boundary")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  geodata::gadm("CAN", level = 0, path = dir, version = "4.1", resolution = 1)
  f <- file.path(dir, "gadm", "gadm41_CAN_0_pk.rds")
  if (!file.exists(f)) {
    stop("geodata::gadm() did not write ", f)
  }
  f
}

## Each tile's DEM file, named by tile; stops naming tiles without one. As a `format = "file"`
## target, it makes the targets built from the DEMs depend on the files' contents.
pipeline_dem_files <- function(demDir, tiles) {
  f <- stats::setNames(pipeline_dem_file(demDir, tiles), as.character(tiles))
  missing <- names(f)[!file.exists(f)]
  if (length(missing) > 0) {
    stop("no DEM tile in ", demDir, " for tile(s) ", paste(missing, collapse = ", "))
  }
  f
}

## Each tile's cells to set to NODATA when its zips are cut, and its land cells (.maskCells()),
## named by tile as `demFiles` is (pipeline_dem_files()).
pipeline_tile_masks <- function(canadaFile, demFiles) {
  canada <- readRDS(canadaFile)
  if (inherits(canada, "PackedSpatVector")) {
    canada <- terra::unwrap(canada)
  }
  lapply(demFiles, function(f) .maskCells(terra::rast(f), canada))
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
