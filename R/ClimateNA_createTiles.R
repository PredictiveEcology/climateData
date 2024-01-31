.allowedClimateTypes <- c("historic", "historic_normals", "future", "future_normals")

#' Paths to ClimateNA tile data
#'
#' - `ClimateNA_tiles_sqlite()`: path to a SQLite database used to look up e.g. urls to climate data
#'   for each tile.
#' - `ClimateNA_tiles_gpkg()`: path to a GeoPackage file defining the tile locations and extents.
#'
#' @return character. file path.
#'
#' @export
#' @rdname ClimateNA_tiles_data
ClimateNA_tiles_sqlite <- function() {
  system.file("extdata/ClimateNA_tiles.sqlite", package = "climateData")
}

#' @export
#' @rdname ClimateNA_tiles_data
ClimateNA_tiles_gpkg <- function() {
  system.file("extdata/tiles.gpkg", package = "climateData")
}

#' Rewrite a `.asc` raster file to use Windows (CR) line-endings
#'
#' @param f character. path to a `.asc` raster file
#'
#' @export
#' @importFrom utils write.table
rewrite_asc <- function(f) {
  readLines(f, warn = FALSE) |>
    utils::write.table(f, row.names = FALSE, col.names = FALSE, quote = FALSE, append = FALSE)

  return(invisible(NULL))
}

#' Get the tile ID of a DEM raster based on its filename
#'
#' @param file character. path to a DEM raster tile file.
#'
#' @export
tileID <- function(file) {
  basename(file) |>
    sub("^can_dem_", "", x = _) |>
    sub("[.]asc$", "", x = _) |>
    as.integer()
}

#' Construct target output paths for ClimateNA data
#'
#' @param dataPath TODO
#'
#' @template ClimateNA_tile
#'
#' @template ClimateNA_type
#'
#' @template ClimateNA_msy
#'
#' @template ClimateNA_gcmssp
#'
#' @return character string representing the path to the target output directory
#'
#' @export
ClimateNA_path <- function(dataPath, tile = NULL, type = NULL, msy = NULL, gcm = NULL, ssp = NULL) {
  stopifnot(type %in% .allowedClimateTypes)

  MSY <-  switch(msy, MSY = "all", M = "monthly", Y = "yearly")
  switch(
    type,
    historic = file.path(dataPath, "historic", MSY, tile),
    historic_normals = file.path(dataPath, "historic", "normals", MSY, tile),
    future = file.path(dataPath, "future", paste0(gcm, "_ssp", ssp), MSY, tile),
    future_normals = file.path(dataPath, "future", "normals", MSY, tile)
  ) |>
    normalizePath(mustWork = FALSE) |>
    (function(x) {
      if (!dir.exists(x)) dir.create(x, recursive = TRUE)
      x
    })()
}

#' Connect to ClimateNA tile sqlite database
#'
#' @template ClimateNA_dbfile
#'
#' @template ClimateNA_type
#'
#' @return named list containing:
#'        - `db`, the database connection;
#'        - `df`, a lazy `tbl_dbi` object (see `dbplyr`).
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom DBI dbCreateTable dbExecute dbExistsTable
#' @importFrom dplyr tbl
#' @importFrom RSQLite SQLite
#' @importFrom tibble rowid_to_column
ClimateNA_sql <- function(dbfile, type) {
  firstRun <- file.exists(dbfile)

  db <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbfile,
    synchronous = "normal",
    extended_types = TRUE ## for DATETIME
  )

  if (isTRUE(firstRun)) {
    DBI::dbExecute(db, "PRAGMA journal_mode = WAL")   ## using WAL allows concurrency
    DBI::dbExecute(db, "PRAGMA busy_timeout = 60000") ## set busy timeout (ms); allows concurrency.
  }

  df_template <- switch(
    type,
    historic_normals = data.frame(
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      period = NA_character_,  ## climate period
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    ),
    future_normals = data.frame(
      gcm = NA_character_,     ## climate scenario GCM
      ssp = NA_character_,     ## climate scenario SSP
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      period = NA_character_,  ## climate period
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    ),
    historic = data.frame(
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      year = NA_character_,    ## climate year
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    ),
    future = data.frame(
      gcm = NA_character_,     ## climate scenario GCM
      ssp = NA_character_,     ## climate scenario SSP
      msy = NA_character_,     ## one of: 'M', 'S', 'Y', 'MSY'
      year = NA_character_,    ## climate year (or period)
      tileid = NA_integer_,    ## tile ID
      created = Sys.time(),    ## timestamp of when tile set was created using ClimateNA
      zipfile = NA_character_, ## relative file path
      archived = Sys.time(),   ## timestamp of when tile set archive was created (zipped)
      gid = NA_character_,     ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = Sys.time(),   ## timestamp of when archive uploaded to google drive
      stringsAsFactors = FALSE
    )
  ) |>
    tibble::rowid_to_column() |> ## add rowid column to use as table primary key
    na.omit()

  tbl <- type

  if (!DBI::dbExistsTable(db, tbl)) {
    DBI::dbCreateTable(db, tbl, df_template)
  }

  df <- dplyr::tbl(db, tbl)

  return(list(db = db, df = df))
}
