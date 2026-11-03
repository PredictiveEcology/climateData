## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# setup ---------------------------------------------------------------------------------------

source("data-raw/01-ClimateNA_setup.R")

runClimateNA <- FALSE ## TRUE
fileChecksums <- TRUE
createZips <- FALSE ## TRUE
uploadArchives <- FALSE ## TRUE
reuploadArchives <- FALSE ## TRUE

climateType <- "historical"

if (isTRUE(fileChecksums)) {
  csdb <- checksums_sql(wrkngDBfile, climateType)
  checksums_db <- csdb[["db"]]
  checksums_historical_df <- csdb[["df"]]
  rm(csdb)
}

dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
climate_db <- dbdf[["db"]]
climate_historical_df <- dbdf[["df"]]
rm(dbdf)

MSYs <- c("MSY", "M", "Y") ## no "S" archives exist: no database rows, no Drive folder below

## only years not yet in the database; available(climateType)[["years"]] for a full rebuild
historical_years <- 2025L
# historical_years <- 1901L:1990L
# historical_years <- 1991L:2024L
historical_decades <- (historical_years %/% 10 * 10) |> unique() |> as.integer()
period_ann <- paste0("Year_", historical_years, ".ann")

tilesToRun <- NULL ## all tiles; or e.g. 26L to check one tile first

if (isTRUE(useClimateNAr)) {
  ClimateNAr_checkYears(historical_years)

  ## Restore each tile's existing archives for the decades written here, so the decade zips below
  ## are rebuilt with all their years, and for the latest year already done, whose outputs give
  ## each tile's grid (for the DEM) and the variables of each archive.
  done <- dplyr::collect(climate_historical_df)
  templateYear <- max(setdiff(done$year, historical_years))
  restore <- done[
    done$msy %in% MSYs &
      (done$year == templateYear | (done$year %/% 10L * 10L) %in% historical_decades),
  ] |>
    dplyr::distinct(msy, tileid, zipfile, gid)
  if (!is.null(tilesToRun)) {
    restore <- restore[restore$tileid %in% tilesToRun, ]
  }
  for (i in seq_len(nrow(restore))) {
    fzip <- file.path(ClimateNAdata, restore$zipfile[i])
    if (!file.exists(fzip)) {
      dir.create(dirname(fzip), recursive = TRUE, showWarnings = FALSE)
      googledrive::drive_download(googledrive::as_id(restore$gid[i]), path = fzip)
    }
    archive::archive_extract(
      fzip,
      dir = ClimateNA_path(ClimateNAdata, tile = restore$tileid[i], type = climateType, restore$msy[i])
    )
  }

  dem_dir <- file.path(ClimateNAdata, "dem_ClimateNAr")
  dir.create(dem_dir, showWarnings = FALSE)
  dem_ff <- vapply(unique(restore$tileid), function(tile) {
    f <- file.path(dem_dir, paste0("can_dem_", tile, ".asc"))
    if (!file.exists(f)) {
      tmpl <- file.path(
        ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, "MSY"),
        paste0("Year_", templateYear, "MSY"),
        "Tmax07.asc"
      )
      ClimateNAr_tileDEM(
        tmpl,
        f,
        destinationPath = file.path(ClimateNAdata, "dem_src"),
        cachePath = file.path(ClimateNAdata, "cache")
      )
    }
    f
  }, character(1))
}

if (!exists("dem_ff")) {
  dem_ff <- list.files(file.path(ClimateNAdata, "dem"), pattern = "[.]asc$", full.names = TRUE)
  stopifnot(length(dem_ff) > 0)
}
if (!is.null(tilesToRun)) {
  dem_ff <- dem_ff[tileID(dem_ff) %in% tilesToRun]
}

# plan(sequential)
plan("callr", workers = min(length(dem_ff), if (isTRUE(useClimateNAr)) maxWorkersClimateNAr else maxCores))

# get ClimateNA historical time series ----------------------------------------------------------

progressr::with_progress({
  p <- progressr::progressor(along = dem_ff)
  new_rows_historic <- future.apply::future_lapply(
    dem_ff,
    function(f) {
      dbdf <- climateData::ClimateNA_sql(wrkngDBfile, climateType)
      climate_db <- dbdf[["db"]]
      climate_historical_df <- dbdf[["df"]]
      rm(dbdf)

      f <- normalizePath(f)
      tile <- climateData::tileID(f)

      z <- lapply(MSYs, function(msy) {
        lapply(period_ann, function(ann) {
          ClimateNAout <- climateData::ClimateNA_path(
            ClimateNAdata,
            tile = tile,
            type = climateType,
            msy
          )

          yr <- as.integer(substr(ann, 6, 9))

          row <- dplyr::filter(
            climate_historical_df,
            msy == !!msy & year == !!yr & tileid == !!tile
          ) |>
            dplyr::collect()

          if (nrow(row) == 0) {
            if (isTRUE(runClimateNA) && isTRUE(useClimateNAr)) {
              ## one ClimateNAr run writes all of MSYs for this year
              if (!dir.exists(file.path(ClimateNAout, paste0("Year_", yr, msy)))) {
                message(glue::glue("Running ClimateNAr for tile {tile}: {ann} ..."))
                outDirs <- sapply(MSYs, function(m) {
                  climateData::ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, m)
                }, simplify = FALSE)
                ## each archive keeps the variables it had for templateYear
                vars <- Map(function(d, m) {
                  list.files(file.path(d, paste0("Year_", templateYear, m)), "[.]asc$") |>
                    tools::file_path_sans_ext()
                }, outDirs, names(outDirs))
                ClimateNAr_tile(ClimateNAr_fn(), f, yr, outDirs, vars, file.path(ClimateNAdata, "tmp"))
                message(glue::glue("... completed ClimateNAr for tile {tile}: {ann}"))
              }
            } else if (isTRUE(runClimateNA)) {
              message(glue::glue("Running ClimateNA for tile {tile}: {ann} {msy} ..."))
              withr::local_dir(ClimateNAdir)
              sys::exec_wait(
                ClimateNAexe,
                args = c(
                  paste0("/", msy),
                  paste0("/", ann),
                  paste0("/", f),
                  paste0("/", ClimateNAout)
                )
              )
              withr::deferred_run()
              message(glue::glue("... completed ClimateNA for tile {tile}: {ann} {msy}"))
            }

            new_row <- data.frame(
              ## id will be filled automatically
              msy = msy,
              year = yr,
              tileid = tile,
              created = file.info(ClimateNAout)$mtime,
              stringsAsFactors = FALSE
            )
            # rows_append(climate_historical_df, new_row, copy = TRUE, in_place = TRUE)
          } else {
            message(glue::glue("Skipping ClimateNA for tile {tile}: {ann} {msy}."))
            new_row <- dplyr::mutate(row, created = file.info(ClimateNAout)$mtime)
            # rows_update(climate_historical_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")
          }

          return(new_row)
        }) |>
          dplyr::bind_rows()
      }) |>
        dplyr::bind_rows()

      DBI::dbDisconnect(climate_db)

      p(sprintf("tile = %g", tile))

      return(z)
    },
    future.seed = NULL
  )
})
new_rows_historic <- dplyr::bind_rows(new_rows_historic)

## new rows have no rowid: SQLite assigns it (AUTOINCREMENT). Numbering them from 1, as before,
## collides with the existing rows.
if (!"rowid" %in% colnames(new_rows_historic)) {
  dplyr::rows_append(climate_historical_df, new_rows_historic, copy = TRUE, in_place = TRUE)
} else {
  dplyr::rows_update(
    climate_historical_df,
    new_rows_historic,
    copy = TRUE,
    in_place = TRUE,
    unmatched = "ignore"
  )
}

DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# checksums -----------------------------------------------------------------------------------

# plan(sequential)
# plan("callr", workers = min(length(dem_ff), maxCores))

progressr::with_progress({
  p <- progressr::progressor(steps = length(dem_ff) * length(MSYs) * length(period_ann))
  checksums_historic <- future.apply::future_lapply(
    dem_ff,
    function(f) {
      tile <- normalizePath(f) |> climateData::tileID()

      z <- lapply(MSYs, function(msy) {
        zz <- lapply(period_ann, function(ann) {
          ClimateNAout <- climateData::ClimateNA_path(
            ClimateNAdata,
            tile = tile,
            type = climateType,
            msy
          )

          yr <- as.integer(substr(ann, 6, 9))

          digs <- file.path(ClimateNAout, paste0(tools::file_path_sans_ext(ann), msy)) |>
            fs::dir_ls(type = "file") |>
            vapply(digest::digest, file = TRUE, algo = "xxhash64", FUN.VALUE = character(1))

          checksums <- data.frame(
            msy = msy,
            year = yr,
            tileid = tile,
            filename = basename(names(digs)),
            filehash = unname(digs),
            stringsAsFactors = FALSE
          )

          p(sprintf("tile = %g; MSY = %s; period_ann = %s", tile, msy, ann))

          return(checksums)
        }) |>
          dplyr::bind_rows()

        return(zz)
      }) |>
        dplyr::bind_rows()

      return(z)
    },
    future.globals = c("ClimateNAdata", "climateType", "MSYs", "period_ann", "p"),
    future.seed = NULL
  )
})
checksums_historic <- dplyr::bind_rows(checksums_historic)

if (!"id" %in% colnames(checksums_historic)) {
  dplyr::rows_append(checksums_historical_df, checksums_historic, copy = TRUE, in_place = TRUE)
} else {
  dplyr::rows_update(
    checksums_historical_df,
    checksums_historic,
    copy = TRUE,
    in_place = TRUE,
    unmatched = "ignore"
  )
}

DBI::dbDisconnect(checksums_db)
DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# archive tilesets ----------------------------------------------------------------------------

if (createZips) {
  # plan(sequential)
  # plan("callr", workers = min(length(dem_ff), maxCores))

  ## historical time series
  progressr::with_progress({
    p <- progressr::progressor(steps = length(dem_ff) * length(MSYs) * length(historical_decades))
    new_rows_historic <- future.apply::future_lapply(
      dem_ff,
      function(f) {
        dbdf <- climateData::ClimateNA_sql(wrkngDBfile, climateType)
        climate_db <- dbdf[["db"]]
        climate_historical_df <- dbdf[["df"]]
        rm(dbdf)

        tile <- normalizePath(f) |> climateData::tileID()

        z <- lapply(MSYs, function(msy) {
          ClimateNAout <- climateData::ClimateNA_path(
            ClimateNAdata,
            tile = tile,
            type = climateType,
            msy
          )

          lapply(historical_decades, function(dcd) {
            fzip <- paste0(ClimateNAout, "_", msy, "_", dcd, ".zip")

            row <- dplyr::filter(
              climate_historical_df,
              msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tile ## zip each decade
            ) |>
              dplyr::collect()

            ## the zip replaces the decade's archive: every year it lists must be on disk
            stopifnot(all(dir.exists(file.path(ClimateNAout, paste0("Year_", row$year, msy)))))

            files2add <- fs::dir_ls(
              ClimateNAout,
              regexp = paste0("Year_(", paste(dcd + 0:9, collapse = "|"), ")", msy, "$")
            ) |>
              fs::dir_ls() |>
              fs::path_rel(ClimateNAout)

            withr::local_dir(ClimateNAout)
            archive::archive_write_files(archive = fzip, files = files2add)
            withr::deferred_run()

            new_row <- dplyr::mutate(
              row,
              archived = file.info(fzip)$mtime,
              zipfile = fs::path_rel(fzip, ClimateNAdata)
            )
            # rows_update(climate_historical_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")

            p(sprintf("tile = %g; MSY = %s; historical_decade = %g", tile, msy, dcd))

            return(new_row)
          }) |>
            dplyr::bind_rows()
        }) |>
          dplyr::bind_rows()

        DBI::dbDisconnect(climate_db)

        return(z)
      },
      future.globals = c("ClimateNAdata", "climateType", "historical_decades", "MSYs", "p", "wrkngDBfile"),
      future.seed = NULL
    )
  })
  new_rows_historic <- dplyr::bind_rows(new_rows_historic)

  ## the connection was closed after the first section
  dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
  climate_db <- dbdf[["db"]]
  climate_historical_df <- dbdf[["df"]]
  rm(dbdf)

  rows_update(
    climate_historical_df,
    new_rows_historic,
    copy = TRUE,
    in_place = TRUE,
    unmatched = "ignore"
  )

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

# upload tilesets -----------------------------------------------------------------------------

if (uploadArchives) {
  plan("callr", workers = 8L) ## don't want too many parallel uploads

  gids_historic <- list(
    MSY = "1r9yZMlXDqEQxBX2aQeDBAkHlzegqZRLL",
    M = "1E3PNXb_ti5a6JSzmBkrRP5UQ-y6Bco7A",
    Y = "1bp6UX5b9nczub0OG41YAhOQUmyerywc6"
  )

  ## historical time series
  progressr::with_progress({
    p <- progressr::progressor(steps = length(dem_ff) * length(MSYs) * length(historical_decades))
    new_rows_historic <- future.apply::future_lapply(
      dem_ff,
      function(f) {
        googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

        dbdf <- climateData::ClimateNA_sql(wrkngDBfile, climateType)
        climate_db <- dbdf[["db"]]
        climate_historical_df <- dbdf[["df"]]
        rm(dbdf)

        tile <- normalizePath(f) |> climateData::tileID()

        z <- lapply(MSYs, function(msy) {
          ClimateNAout <- climateData::ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy)

          gid <- googledrive::as_id(gids_historic[[msy]])
          drivefiles <- googledrive::drive_ls(gid)

          lapply(historical_decades, function(dcd) {
            fzip <- paste0(ClimateNAout, "_", msy, "_", dcd, ".zip")

            row <- dplyr::filter(
              climate_historical_df,
              msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tile ## zip each decade
            ) |>
              dplyr::collect()

            if (reuploadArchives) {
              gt <- googledrive::drive_put(media = fzip, path = googledrive::as_id(gid))
            } else {
              gt <- dplyr::filter(drivefiles, name == basename(fzip))

              if (nrow(gt) == 0) {
                gt <- googledrive::drive_put(media = fzip, path = googledrive::as_id(gid))
              }
            }

            new_row <- dplyr::mutate(row, uploaded = Sys.time(), gid = gt$id)
            # rows_update(climate_hist_normals_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")

            p(sprintf("tile = %g; MSY = %s; historical_decade = %g", tile, msy, dcd))

            return(new_row)
          }) |>
            dplyr::bind_rows()
        }) |>
          dplyr::bind_rows()

        DBI::dbDisconnect(climate_db)

        return(z)
      },
      future.globals = c("ClimateNAdata", "climateType", "gids_historic", "historical_decades",
                         "MSYs", "oauthCachePath", "p", "reuploadArchives", "userEmail", "wrkngDBfile"),
      future.seed = NULL
    )
  })
  new_rows_historic <- dplyr::bind_rows(new_rows_historic)

  dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
  climate_db <- dbdf[["db"]]
  climate_historical_df <- dbdf[["df"]]
  rm(dbdf)

  rows_update(
    climate_historical_df,
    new_rows_historic,
    copy = TRUE,
    in_place = TRUE,
    unmatched = "ignore"
  )

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

## copy updated db to package data folder + remove working copy
file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
file.copy(wrkngDBfile, pkgDBfile, overwrite = TRUE)
unlink(wrkngDBfile)
