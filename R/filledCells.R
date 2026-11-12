#' Land cells filled from a nearby point
#'
#' ClimateNA writes no values at some land cells: its future climate data miss some coastal
#' cells, and a few cells get no values at all. In the archives, each such cell holds
#' ClimateNA's values for the nearest land cell it covers (a median of about 5 km away),
#' computed at the empty cell's own elevation. `filledCells()` lists those cells, so they can be
#' mapped, or masked out of an analysis.
#'
#' `class` says which archives a cell was filled in: `"historical"` (historical years and
#' normals) or `"future"` (future years and normals). A cell with `filled = FALSE` had no
#' covered land cell within reach, and has no values.
#'
#' @param studyArea optional `sf`, `sfc` or `SpatVector` polygon(s), in any CRS: only cells
#'   inside are returned.
#' @param tiles optional integer tile IDs.
#'
#' @return an `sf` of points (the cells' centres, EPSG:4326) with columns `class`, `tile`,
#'   `cell` (the cell number in the tile's grid, row by row from the top left), `elev` (the
#'   cell's elevation, m), `filled`, `src_lon` and `src_lat` (where ClimateNA was run) and `km`
#'   (the distance to it). No rows for a data release without filled cells.
#'
#' @export
#' @examples
#' \donttest{
#' fc <- filledCells()
#' table(fc$class, fc$filled)
#' }
filledCells <- function(studyArea = NULL, tiles = NULL) {
  db <- sqlite_connect_db(ClimateNA_tiles_sqlite(), readonly = TRUE)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  x <- if (DBI::dbExistsTable(db, "filled_cells")) {
    DBI::dbGetQuery(
      db,
      "SELECT class, tileid, cell, lon, lat, elev, src_lon, src_lat, km
       FROM filled_cells ORDER BY class, tileid, cell"
    )
  } else {
    data.frame(
      class = character(0),
      tileid = integer(0),
      cell = integer(0),
      lon = numeric(0),
      lat = numeric(0),
      elev = numeric(0),
      src_lon = numeric(0),
      src_lat = numeric(0),
      km = numeric(0)
    )
  }
  names(x)[names(x) == "tileid"] <- "tile"
  if (!is.null(tiles)) {
    x <- x[x$tile %in% as.integer(tiles), , drop = FALSE]
  }
  x$filled <- !is.na(x$src_lon)
  x <- x[c("class", "tile", "cell", "lon", "lat", "elev", "filled", "src_lon", "src_lat", "km")]
  ## built from sfc points: st_as_sf(coords =) warns when there are none
  pts <- sf::st_sf(
    x[setdiff(names(x), c("lon", "lat"))],
    geometry = sf::st_sfc(
      lapply(seq_len(nrow(x)), \(i) sf::st_point(c(x$lon[i], x$lat[i]))),
      crs = 4326
    )
  )

  if (!is.null(studyArea)) {
    if (inherits(studyArea, "SpatVector")) {
      studyArea <- sf::st_as_sf(studyArea)
    }
    within <- sf::st_intersects(sf::st_transform(pts, sf::st_crs(studyArea)), studyArea)
    pts <- pts[lengths(within) > 0, ]
  }
  rownames(pts) <- NULL
  pts
}
