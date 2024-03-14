## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# setup ---------------------------------------------------------------------------------------

source("data-raw/01-ClimateNA_setup.R")

runClimateNA <- FALSE ## TRUE
fileChecksums <- TRUE
createZips <- FALSE ## TRUE
uploadArchives <- FALSE ## TRUE

climateType <- "historic"

if (isTRUE(fileChecksums)) {
  csdb <- checksums_sql(wrkngDBfile, climateType)
  checksums_db <- csdb[["db"]]
  checksums_historic_df <- csdb[["df"]]
  rm(csdb)
}

dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
climate_db <- dbdf[["db"]]
climate_historic_df <- dbdf[["df"]]
rm(dbdf)

MSYs <- c("MSY", "M", "S", "Y")

historic_years <- available(climateType)[["years"]]
# historic_years <- 1901L:1990L
# historic_years <- 1991L:2022L
historic_decades <- (historic_years %/% 10 * 10) |> unique() |> as.integer()
period_ann <- paste0("Year_", historic_years, ".ann")

if (!exists("dem_ff")) {
  dem_ff <- list.files(file.path(ClimateNAdata, "dem"), pattern = "[.]asc$", full.names = TRUE)
  stopifnot(length(dem_ff) > 0)
}

plan("callr", workers = min(length(dem_ff), parallelly::availableCores() / 2))

# get ClimateNA historic time series ----------------------------------------------------------

new_rows_historic <- future_lapply(dem_ff, function(f) {
  dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
  climate_db <- dbdf[["db"]]
  climate_historic_df <- dbdf[["df"]]
  rm(dbdf)

  f <- normalizePath(f)
  tile <- tileID(f)

  z <- lapply(MSYs, function(msy) {
    lapply(period_ann, function(ann) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy)

      yr <- substr(ann, 6, 9)

      row <- dplyr::filter(
        climate_historic_df,
        msy == !!msy & year == !!yr & tileid == !!tile
      ) |>
        collect()

      if (nrow(row) == 0) {
        if (isTRUE(runClimateNA)) {
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
        }

        new_row <- data.frame(
          ## id will be filled automatically
          msy = msy,
          year = yr,
          tileid = tile,
          created = file.info(ClimateNAout)$mtime,
          stringsAsFactors = FALSE
        )
        # rows_append(climate_historic_df, new_row, copy = TRUE, in_place = TRUE)
      } else {
        new_row <- dplyr::mutate(row, created = file.info(ClimateNAout)$mtime)
        # rows_update(climate_historic_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")
      }

      return(new_row)
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  DBI::dbDisconnect(climate_db)

  return(z)
}, future.seed = NULL) |>
  dplyr::bind_rows()

if (!"rowid" %in% colnames(new_rows_historic)) {
  new_rows_historic <- tibble::rowid_to_column(new_rows_historic)
  rows_append(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE)
} else {
  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# checksums -----------------------------------------------------------------------------------

checksums_historic <- future_lapply(dem_ff, function(f) {
  tile <- normalizePath(f) |> tileID()

  lapply(MSYs, function(msy) {
    lapply(period_ann, function(ann) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy)

      yr <- substr(ann, 6, 9)

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

      return(checksums)
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()
}, future.seed = NULL) |>
  dplyr::bind_rows()

if (!"id" %in% colnames(new_rows_historic)) {
  rows_append(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE)
} else {
  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

DBI::dbDisconnect(checksums_db)
DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# archive tilesets ----------------------------------------------------------------------------

if (createZips) {
  ## historic time series
  new_rows_historic <- future_lapply(dem_ff, function(f) {
    dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
    climate_db <- dbdf[["db"]]
    climate_historic_df <- dbdf[["df"]]
    rm(dbdf)

    tile <- normalizePath(f) |> tileID()

    z <- lapply(MSYs, function(msy) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy)

      lapply(historic_decades, function(dcd) {
        fzip <- paste0(ClimateNAout, "_", msy, "_", dcd, ".zip")

        row <- dplyr::filter(
          climate_historic_df,
          msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tile ## zip each decade
        ) |>
          collect()

        files2add <- fs::dir_ls(ClimateNAout, regexp = paste0("Year_(", paste(dcd + 0:9, collapse = "|"), ")", msy, "$")) |>
          fs::dir_ls() |>
          fs::path_rel(ClimateNAout)

        withr::local_dir(ClimateNAout)
        archive_write_files(archive = fzip, files = files2add)
        withr::deferred_run()

        new_row <- dplyr::mutate(row, archived = file.info(fzip)$mtime, zipfile = fs::path_rel(fzip, ClimateNAdata))
        # rows_update(climate_historic_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")

        return(new_row)
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()

    DBI::dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")

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

  ## historic time series
  new_rows_historic <- future_lapply(dem_ff, function(f) {
    googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

    dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
    climate_db <- dbdf[["db"]]
    climate_historic_df <- dbdf[["df"]]
    rm(dbdf)

    tile <- normalizePath(f) |> tileID()

    z <- lapply(MSYs, function(msy) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy)

      gid <- googledrive::as_id(gids_historic[[msy]])
      drivefiles <- googledrive::drive_ls(gid)

      lapply(historic_decades, function(dcd) {
        fzip <- paste0(ClimateNAout, "_", msy, "_", dcd, ".zip")

        row <- dplyr::filter(
          climate_historic_df,
          msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tile ## zip each decade
        ) |>
          collect()

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

        return(new_row)
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()

    DBI::dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

## copy updated db to package data folder + remove working copy
file.copy(addlDBfile, pkgDBfile, overwrite = TRUE)
unlink(wrkngDBfile)
