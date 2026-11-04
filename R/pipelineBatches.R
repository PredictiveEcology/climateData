## Batch specs and ClimateNA names for the data pipeline (data-raw/pipeline/).
##
## A batch is the unit that becomes one zip per subset: a tile's decade (historical, future) or
## all of a tile's normals periods. Each spec is a plain list whose content alone identifies it,
## so the pipeline's dynamic branches keep their identity when years, GCMs or SSPs are added.

.batchTypes <- c("historical", "future", "historical_normals", "future_normals")

## The folder ClimateNA writes for one period (checked against ClimateNA v7.70 output).
.periodFolder <- function(type, period, msy, gcm = NULL, ssp = NULL) {
  stopifnot(type %in% .batchTypes)
  switch(
    type,
    historical = paste0("Year_", period, msy),
    future = paste0(gcm, "_ssp", ssp, "@", period, msy),
    historical_normals = paste0("Normal_", period, msy),
    future_normals = paste0(gcm, "_ssp", ssp, "_", gsub("_", "-", period), msy)
  )
}

## The period argument ClimateNA takes for one period.
.periodArg <- function(type, period, gcm = NULL, ssp = NULL) {
  stopifnot(type %in% .batchTypes)
  switch(
    type,
    historical = paste0("Year_", period, ".ann"),
    future = paste0(gcm, "_ssp", ssp, "@", period, ".gcm"),
    historical_normals = paste0("Normal_", period, ".nrm"),
    future_normals = paste0(gcm, "_ssp", ssp, "_", gsub("_", "-", period), ".gcm")
  )
}

## One spec per batch: list(id, type, tile, gcm, ssp, msy, decade, periods), ordered by tile,
## gcm, ssp and decade. Historical and future batches hold a decade's years; normals batches
## hold all of a tile's (or tile-scenario's) periods.
.batchSpecs <- function(type, tiles, gcms = NULL, ssps = NULL, years = NULL, periods = NULL) {
  stopifnot(type %in% .batchTypes)
  future <- type %in% c("future", "future_normals")
  normals <- type %in% c("historical_normals", "future_normals")
  scen <- if (future) {
    expand.grid(gcm = gcms, ssp = as.character(ssps), stringsAsFactors = FALSE)
  } else {
    data.frame(gcm = NA_character_, ssp = NA_character_)
  }
  scen <- scen[order(scen$gcm, scen$ssp), , drop = FALSE]

  specs <- list()
  for (tile in sort(as.integer(tiles))) {
    for (s in seq_len(nrow(scen))) {
      gcm <- scen$gcm[s]
      ssp <- scen$ssp[s]
      prefix <- if (future) paste(type, tile, gcm, ssp, sep = "_") else paste(type, tile, sep = "_")
      if (normals) {
        specs[[length(specs) + 1]] <- list(
          id = prefix,
          type = type,
          tile = tile,
          gcm = gcm,
          ssp = ssp,
          msy = "Y",
          decade = NA_integer_,
          periods = periods
        )
      } else {
        yrs <- as.integer(years)
        for (dcd in sort(unique(yrs %/% 10L * 10L))) {
          specs[[length(specs) + 1]] <- list(
            id = paste(prefix, dcd, sep = "_"),
            type = type,
            tile = tile,
            gcm = gcm,
            ssp = ssp,
            msy = "MSY",
            decade = dcd,
            periods = yrs[yrs %/% 10L * 10L == dcd]
          )
        }
      }
    }
  }
  specs
}

## Tile IDs that have a DEM tile (can_dem_<id>.asc) in `demDir`.
.demTiles <- function(demDir) {
  f <- list.files(demDir, pattern = "^can_dem_[0-9]+[.]asc$")
  sort(as.integer(sub("^can_dem_([0-9]+)[.]asc$", "\\1", f)))
}
