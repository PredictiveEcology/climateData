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

  MSY <-  switch(msy, MSY = "all", M = "monthly", S = "seasonal", Y = "yearly")
  switch(
    type,
    historical = file.path(dataPath, "historical", MSY, tile),
    historical_normals = file.path(dataPath, "historical", "normals", MSY, tile),
    future = file.path(dataPath, "future", paste0(gcm, "_ssp", ssp), MSY, tile),
    future_normals = file.path(dataPath, "future",  paste0(gcm, "_ssp", ssp), "normals", MSY, tile)
  ) |>
    normalizePath(mustWork = FALSE) |>
    (function(x) {
      if (!dir.exists(x)) dir.create(x, recursive = TRUE)
      x
    })()
}

#' @keywords internal
#' @importFrom DBI dbExecute
#' @importFrom RSQLite dbConnect SQLite
sqlite_connect_db <- function(dbfile) {
  firstRun <- file.exists(dbfile)

  db <- RSQLite::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbfile,
    synchronous = "normal",
    extended_types = TRUE ## for DATETIME
  )

  if (isTRUE(firstRun)) {
    DBI::dbExecute(db, "PRAGMA journal_mode = WAL")   ## using WAL allows concurrency
    DBI::dbExecute(db, "PRAGMA busy_timeout = 60000") ## set busy timeout (ms); allows concurrency.
  }

  return(db)
}

#' Connect to ClimateNA tile and checksums database
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
#' @importFrom DBI dbCreateTable dbExecute dbExistsTable
#' @importFrom dplyr tbl
#' @importFrom RSQLite dbConnect SQLite
#' @rdname ClimateNA_sql
ClimateNA_sql <- function(dbfile, type) {
  db <- sqlite_connect_db(dbfile)

  df_template <- switch(
    type,
    historical_normals = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      msy = "CHARACTER",       ## one of: 'M', 'S', 'Y', 'MSY'
      period = "CHARACTER",    ## climate period
      tileid = "INTEGER",      ## tile ID
      created = "DATETIME",    ## timestamp of when tile set was created using ClimateNA
      zipfile = "CHARACTER",   ## relative file path
      archived = "DATETIME",   ## timestamp of when tile set archive was created (zipped)
      gid = "CHARACTER",       ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = "DATETIME"    ## timestamp of when archive uploaded to google drive
    ),
    future_normals = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      gcm = "CHARACTER",       ## climate scenario GCM
      ssp = "CHARACTER",       ## climate scenario SSP
      msy = "CHARACTER",       ## one of: 'M', 'S', 'Y', 'MSY'
      period = "CHARACTER",    ## climate period
      tileid = "INTEGER",      ## tile ID
      created = "DATETIME",    ## timestamp of when tile set was created using ClimateNA
      zipfile = "CHARACTER",   ## relative file path
      archived = "DATETIME",   ## timestamp of when tile set archive was created (zipped)
      gid = "CHARACTER",       ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = "DATETIME"    ## timestamp of when archive uploaded to google drive
    ),
    historical = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      msy = "CHARACTER",       ## one of: 'M', 'S', 'Y', 'MSY'
      year = "CHARACTER",      ## climate year
      tileid = "INTEGER",      ## tile ID
      created = "DATETIME",    ## timestamp of when tile set was created using ClimateNA
      zipfile = "CHARACTER",   ## relative file path
      archived = "DATETIME",   ## timestamp of when tile set archive was created (zipped)
      gid = "CHARACTER",       ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = "DATETIME"    ## timestamp of when archive uploaded to google drive
    ),
    future = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      gcm = "CHARACTER",       ## climate scenario GCM
      ssp = "CHARACTER",       ## climate scenario SSP
      msy = "CHARACTER",       ## one of: 'M', 'S', 'Y', 'MSY'
      year = "CHARACTER",      ## climate year (or period)
      tileid = "INTEGER",      ## tile ID
      created = "DATETIME",    ## timestamp of when tile set was created using ClimateNA
      zipfile = "CHARACTER",   ## relative file path
      archived = "DATETIME",   ## timestamp of when tile set archive was created (zipped)
      gid = "CHARACTER",       ## google drive file id; archives built by decade, so there will be dupe gids
      uploaded = "DATETIME"    ## timestamp of when archive uploaded to google drive
    )
  )

  tbl <- type

  if (!DBI::dbExistsTable(db, tbl)) {
    DBI::dbCreateTable(db, tbl, df_template)
  }

  df <- dplyr::tbl(db, tbl)

  return(list(db = db, df = df))
}

#' @export
#' @rdname ClimateNA_sql
checksums_sql <- function(dbfile, type) {
  db <- sqlite_connect_db(dbfile)

  df_template <- switch(
    type,
    historical_normals = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      msy = "CHARACTER",        ## one of: 'M', 'S', 'Y', 'MSY'
      period = "CHARACTER",     ## climate period
      tileid = "ISNTEGER",      ## tile ID
      filename = "CHARACTER",   ## file name
      filehash = "CHARACTER"    ## file hash (checksum)
    ),
    future_normals = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      gcm = "CHARACTER",        ## climate scenario GCM
      ssp = "CHARACTER",        ## climate scenario SSP
      msy = "CHARACTER",        ## one of: 'M', 'S', 'Y', 'MSY'
      period = "CHARACTER",     ## climate period
      tileid = "INTEGER",       ## tile ID
      filename = "CHARACTER",   ## file name
      filehash = "CHARACTER"    ## file hash (checksum)
    ),
    historical = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      msy = "CHARACTER",        ## one of: 'M', 'S', 'Y', 'MSY'
      year = "CHARACTER",       ## climate year
      tileid = "INTEGER",       ## tile ID
      filename = "CHARACTER",   ## file name
      filehash = "CHARACTER"    ## file hash (checksum)
    ),
    future = c(
      id = "INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE",
      gcm = "CHARACTER",        ## climate scenario GCM
      ssp = "CHARACTER",        ## climate scenario SSP
      msy = "CHARACTER",        ## one of: 'M', 'S', 'Y', 'MSY'
      year = "CHARACTER",       ## climate year (or period)
      tileid = "INTEGER",       ## tile ID
      filename = "CHARACTER",   ## file name
      filehash = "CHARACTER"    ## file hash (checksum)
    )
  )

  tbl <- paste0("checksums_", type)

  if (!DBI::dbExistsTable(db, tbl)) {
    DBI::dbCreateTable(db, tbl, df_template)
  }

  df <- dplyr::tbl(db, tbl)

  return(list(db = db, df = df))
}
