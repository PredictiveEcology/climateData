## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process historic, projected, and normals data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# setup ---------------------------------------------------------------------------------------

source("data-raw/01-ClimateNA_setup.R")

runClimateNA <- FALSE ## TRUE
fileChecksums <- FALSE ## TRUE
createZips <- FALSE ## TRUE
uploadArchives <- FALSE ## TRUE
reuploadArchives <- FALSE ## TRUE

climateType <- "future"

if (isTRUE(fileChecksums)) {
  csdb <- checksums_sql(wrkngDBfile, climateType)
  checksums_db <- csdb[["db"]]
  checksums_future_df <- csdb[["df"]]
  rm(csdb)
}

dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
climate_db <- dbdf[["db"]]
future_climate_df <- dbdf[["df"]]
rm(dbdf)

MSYs <- c("MSY", "M", "S", "Y")
GCMs <- available(climateType)[["gcms"]]
SSPs <- available(climateType)[["ssps"]]
future_years <- available(climateType)[["years"]]
future_decades <- (future_years %/% 10 * 10) |> unique() |> as.integer()

if (!exists("dem_ff")) {
  dem_ff <- list.files(file.path(ClimateNAdata, "dem"), pattern = "[.]asc$", full.names = TRUE)
  stopifnot(length(dem_ff) > 0)
}

plan("callr", workers = min(length(dem_ff), maxCores))

# get ClimateNA future time series ------------------------------------------------------------

new_rows_future <- future_lapply(dem_ff, function(f) {
  dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
  climate_db <- dbdf[["db"]]
  future_climate_df <- dbdf[["df"]]
  rm(dbdf)

  f <- normalizePath(f)
  tile <- tileID(f)

  z <- lapply(GCMs, function(gcm) {
    lapply(SSPs, function(ssp) {
      lapply(MSYs, function(msy) {
        ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)

        lapply(future_years, function(yr) {
          row <- dplyr::filter(
            future_climate_df,
            gcm == !!gcm & ssp == !!ssp & msy == !!msy & year == !!yr & tileid == !!tile
          ) |>
            collect()

          if (nrow(row) == 0) {
            if (isTRUE(runClimateNA)) {
              withr::local_dir(ClimateNAdir)
              sys::exec_wait(
                ClimateNAexe,
                args = c(
                  paste0("/", msy),
                  paste0("/", gcm, "_ssp", ssp, "@", yr, ".gcm"),
                  paste0("/", f),
                  paste0("/", ClimateNAout)
                )
              )
              withr::deferred_run()
            }

            new_row <- data.frame(
              gcm = gcm,
              ssp = ssp,
              msy = msy,
              year = yr,
              tileid = tile,
              created = file.info(ClimateNAout)$mtime,
              stringsAsFactors = FALSE
            )
            # rows_append(future_climate_df, new_row, copy = TRUE, in_place = TRUE)
          } else {
            new_row <- dplyr::mutate(row, created = file.info(ClimateNAout)$mtime)
            # rows_update(future_climate_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")
          }

          return(new_row)
        }) |>
          dplyr::bind_rows()
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  DBI::dbDisconnect(climate_db)

  return(z)
}, future.seed = NULL) |>
  dplyr::bind_rows()

if (!"rowid" %in% colnames(new_rows_future)) {
  new_rows_future <- tibble::rowid_to_column(new_rows_future)
  rows_append(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE)
} else {
  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

  DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# checksums -----------------------------------------------------------------------------------

checksums_future <- future_lapply(dem_ff, function(f) {
  tile <- normalizePath(f) |> tileID()

  lapply(GCMs, function(gcm) {
    lapply(SSPs, function(ssp) {
      lapply(MSYs, function(msy) {
        ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)

        lapply(future_years, function(yr) {
          digs <- file.path(ClimateNAout, paste0(tools::file_path_sans_ext(nrm), "Y")) |>
            fs::dir_ls(type = "file") |>
            vapply(digest::digest, file = TRUE, algo = "xxhash64", FUN.VALUE = character(1))

          checksums <- data.frame(
            gcm = gcm,
            ssp = ssp,
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
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()
}, future.seed = NULL) |>
  dplyr::bind_rows()

if (!"id" %in% colnames(new_rows_future)) {
  rows_append(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE)
} else {
  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

DBI::dbDisconnect(checksums_db)
DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# archive tilesets ----------------------------------------------------------------------------

if (createZips) {
  ## future time series
  new_rows_future <- future_lapply(dem_ff, function(f) {
    dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
    climate_db <- dbdf[["db"]]
    future_climate_df <- dbdf[["df"]]
    rm(dbdf)

    tile <- normalizePath(f) |> tileID()

    z <- lapply(GCMs, function(gcm) {
      lapply(SSPs, function(ssp) {
        lapply(MSYs, function(msy) {
          lapply(future_decades, function(dcd) {
            ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)
            fzip <- paste0(ClimateNAout, "_", gcm, "_", ssp, "_", msy, "_", dcd, ".zip")

            type <- paste0(climateType, "_", msy)

            row <- dplyr::filter(
              future_climate_df,
              gcm == !!gcm & ssp == !!ssp & msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tile
            ) |>
              collect()

            files2add <- fs::dir_ls(ClimateNAout, regexp = paste0("@(", paste(dcd + 0:9, collapse = "|"), ")", msy, "$")) |>
              fs::dir_ls() |>
              fs::path_rel(ClimateNAout)

            withr::local_dir(ClimateNAout)
            archive_write_files(archive = fzip, files = files2add)
            withr::deferred_run()

            new_row <- dplyr::mutate(row, archived = file.info(fzip)$mtime, zipfile = fs::path_rel(fzip, ClimateNAdata))
            # rows_update(future_climate_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")

            return(new_row)
          }) |>
            dplyr::bind_rows()
        }) |>
          dplyr::bind_rows()
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()

    DBI::dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

# upload tilesets -----------------------------------------------------------------------------

if (uploadArchives) {
  googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

  plan("callr", workers = 16L) ## don't want too many parallel uploads

  gids_future <- googledrive::drive_ls(googledrive::as_id("1DH_JF6ZluYsZUpHGRXpXI8Q-DOUOE9AY")) |>
    dplyr::select(name, id) |>
    dplyr::mutate(gcm = sapply(name, function(x) strsplit(x, "_")[[1]][1]),
                  ssp = sapply(name, function(x) strsplit(x, "(_ssp|_RCP)")[[1]][2]),
                  name = NULL,
                  .before = id)

  ## future time series
  new_rows_future <- future_lapply(dem_ff, function(f) {
    dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
    climate_db <- dbdf[["db"]]
    future_climate_df <- dbdf[["df"]]
    rm(dbdf)

    tile <- normalizePath(f) |> tileID()

    z <- lapply(GCMs, function(gcm) {
      lapply(SSPs, function(ssp) {
        lapply(MSYs, function(msy) {
          ## re-auth to avoid curl::curl_fetch_memory() "Error in the HTTP2 framing layer"
          googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

          gid <- dplyr::filter(gids_future, gcm == !!gcm, ssp == !!ssp)[["id"]]
          drivefiles <- googledrive::drive_ls(gid)

          lapply(future_decades, function(dcd) {
            ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)
            fzip <- paste0(ClimateNAout, "_", gcm, "_", ssp, "_", msy, "_", dcd, ".zip")

            type <- paste0(climateType, "_", msy)

            row <- dplyr::filter(
              future_climate_df,
              gcm == !!gcm & ssp == !!ssp & msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tile
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
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()

    DBI::dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

## copy updated db to package data folder + remove working copy
file.copy(addlDBfile, pkgDBfile, overwrite = TRUE)
unlink(wrkngDBfile)
