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
  ## read-only, in place: called on every run of code that caches, often by many processes at once
  db <- sqlite_connect_db(ClimateNA_tiles_sqlite(), readonly = TRUE)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

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
