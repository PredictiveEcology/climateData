#' Identify the climate data release behind the installed package
#'
#' Each climateData release ships a tile index pointing at one release of the ClimateNA
#' archives on Google Drive. Use this to tell releases apart, e.g. in the cache key of code that
#' caches `prepClimateLayers()` results, so a new data release isn't served from an old cache.
#'
#' @return a list with `release_id` (e.g. `"cna-v7.70-3arcmin"`), `climatena_version` (the
#'   ClimateNA release that produced the data) and `dem_resolution` (e.g. `"3arcmin"`). Indexes
#'   made before releases were recorded give `release_id = "pre-3.0.0"`.
#'
#' @export
#' @examples
#' climateDataRelease()$release_id
climateDataRelease <- function() {
  ## read a copy: the index is in WAL mode, and even a read-only connection leaves -wal/-shm
  ## files next to the file it opens
  f <- tempfile(fileext = ".sqlite")
  on.exit(unlink(paste0(f, c("", "-wal", "-shm"))), add = TRUE)
  file.copy(ClimateNA_tiles_sqlite(), f)
  db <- DBI::dbConnect(RSQLite::SQLite(), f, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(db), add = TRUE, after = FALSE)

  if (!DBI::dbExistsTable(db, "release")) {
    return(list(
      release_id = "pre-3.0.0",
      climatena_version = NA_character_,
      dem_resolution = "3arcmin"
    ))
  }
  r <- DBI::dbGetQuery(
    db,
    "SELECT release_id, climatena_version, dem_resolution FROM release LIMIT 1"
  )
  as.list(r[1, ])
}
