## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# setup ---------------------------------------------------------------------------------------

source("data-raw/01-ClimateNA_setup.R")

runClimateNA <- FALSE ## TRUE
fileChecksums <- FALSE ## TRUE
createZips <- FALSE ## TRUE
uploadArchives <- FALSE ## TRUE

climateType <- "future_normals"

if (isTRUE(fileChecksums)) {
  csdb <- checksums_sql(wrkngDBfile, climateType)
  checksums_db <- csdb[["db"]]
  checksums_futu_normals_df <- csdb[["df"]]
  rm(csdb)
}

dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
climate_db <- dbdf[["db"]]
climate_futu_normals_df <- dbdf[["df"]]
rm(dbdf)

MSYs <- c("Y")
GCMs <- available(climateType)[["gcms"]]
SSPs <- available(climateType)[["ssps"]]
future_nrm_prds <- available(climateType)[["periods"]]
future_nrm <- paste0(future_nrm_prds, ".gcm") ## TODO: need to combine gcm, ssp, period

if (!exists("dem_ff")) {
  dem_ff <- list.files(file.path(ClimateNAdata, "dem"), pattern = "[.]asc$", full.names = TRUE)
  stopifnot(length(dem_ff) > 0)
}

plan("callr", workers = min(length(dem_ff), parallelly::availableCores() / 2))

# get ClimateNA future normals --------------------------------------------------------------

new_rows_futu_normals <- future_lapply(dem_ff, function(f) {
  dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
  climate_db <- dbdf[["db"]]
  climate_futu_normals_df <- dbdf[["df"]]
  rm(dbdf)

  f <- normalizePath(f)
  tile <- tileID(f)

  z <- lapply(MSYs, function(msy) {
    lapply(future_nrm, function(nrm) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy = msy)

      period <- substr(nrm, 8, 16)

      row <- dplyr::filter(
        climate_futu_normals_df,
        msy == !!msy & period == !!period & tileid == !!tile
      ) |>
        collect()

      if (nrow(row) == 0) {
        if (isTRUE(runClimateNA)) {
          withr::local_dir(ClimateNAdir)
          system2(ClimateNAexe,
                  args = c(
                    paste0("/", msy),
                    paste0("/", nrm),
                    paste0("/", f),
                    paste0("/", ClimateNAout)
                  ))
          withr::deferred_run()
        }

        new_row <- data.frame(
          msy = msy,
          period = period,
          tileid = tile,
          created = file.info(ClimateNAout)$mtime,
          stringsAsFactors = FALSE
        )
        # rows_append(climate_futu_normals_df, new_row, copy = TRUE, in_place = TRUE)
      } else {
        new_row <- dplyr::mutate(row, created = file.info(ClimateNAout)$mtime)
        # rows_update(climate_futu_normals_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")
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

if (!"rowid" %in% colnames(new_rows_futu_normals)) {
  new_rows_futu_normals <- tibble::rowid_to_column(new_rows_futu_normals)
  rows_append(climate_futu_normals_df, new_rows_futu_normals, copy = TRUE, in_place = TRUE)
} else {
  rows_update(climate_futu_normals_df, new_rows_futu_normals, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# checksums -----------------------------------------------------------------------------------

checksums_futu_normals <- future_lapply(dem_ff, function(f) {
  tile <- normalizePath(f) |> tileID()

  lapply(GCMs, function(gcm) {
    lapply(SSPs, function(ssp) {
      lapply(MSYs, function(msy) {
        lapply(future_nrm, function(nrm) {
          ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)

          period <- substr(nrm, 8, 16)

          digs <- file.path(ClimateNAout, paste0(tools::file_path_sans_ext(nrm), "Y")) |>
            fs::dir_ls(type = "file") |>
            vapply(digest::digest, file = TRUE, algo = "xxhash64", FUN.VALUE = character(1))

          checksums <- data.frame(
            msy = msy,
            period = period,
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

if (!"id" %in% colnames(new_rows_futu_normals)) {
  rows_append(climate_futu_normals_df, new_rows_futu_normals, copy = TRUE, in_place = TRUE)
} else {
  rows_update(climate_futu_normals_df, new_rows_futu_normals, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

DBI::dbDisconnect(checksums_db)
DBI::dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# archive tilesets ----------------------------------------------------------------------------

if (createZips) {
  ## future normals
  new_rows_futu_normals <- future_lapply(dem_ff, function(f) {
    dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
    climate_db <- dbdf[["db"]]
    climate_futu_normals_df <- dbdf[["df"]]
    rm(dbdf)

    tile <- normalizePath(f) |> tileID()

    z <- lapply(GCMs, function(gcm) {
      lapply(SSPs, function(ssp) {
        lapply(MSYs, function(msy) {
          lapply(future_nrm, function(nrm) {
            ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)

            fzip <- paste0(ClimateNAout, "_normals_", msy, ".zip")

            row <- dplyr::filter(
              climate_futu_normals_df,
              gcm == !!gcm & ssp == !!ssp & msy == !!msy & period %in% !!period & tileid == !!tile
            ) |>
              collect()

            archive_write_dir(archive = fzip, dir = ClimateNAout)

            new_row <- dplyr::mutate(row, archived = file.info(fzip)$mtime, zipfile = fs::path_rel(fzip, ClimateNAdata))
            # rows_update(climate_futu_normals_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")

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

  rows_update(climate_futu_normals_df, new_rows_futu_normals, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

# upload tilesets -----------------------------------------------------------------------------

if (uploadArchives) {
  plan("callr", workers = 8L) ## don't want too many parallel uploads

  gids_futu_normals <- list(
    Y = "1FStzgwcLMz4gky2UJtt9z5U8MWR1A94k"
  )

  ## future normals
  new_rows_futu_normals <- future_lapply(dem_ff, function(f) {
    googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

    dbdf <- ClimateNA_sql(wrkngDBfile, climateType)
    climate_db <- dbdf[["db"]]
    climate_futu_normals_df <- dbdf[["df"]]
    rm(dbdf)

    tile <- normalizePath(f) |> tileID()

    z <- lapply(GCMs, function(gcm) {
      lapply(SSPs, function(ssp) {
        lapply(MSYs, function(msy) {
          lapply(future_nrm, function(nrm) {
            ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tile, type = climateType, msy, gcm, ssp)

            fzip <- paste0(ClimateNAout, "_normals_", msy, ".zip")

            row <- dplyr::filter(
              climate_futu_normals_df,
              gcm == !!gcm & ssp == !!ssp & msy == !!msy & period %in% !!period & tileid == !!tile
            ) |>
              collect()

            gt <- googledrive::drive_put(media = fzip, path = googledrive::as_id(gids_futu_normals[[msy]]))

            new_row <- dplyr::mutate(row, uploaded = Sys.time(), gid = gt$id)
            # rows_update(climate_futu_normals_df, new_row, copy = TRUE, in_place = TRUE, unmatched = "ignore")

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

  rows_update(climate_futu_normals_df, new_rows_futu_normals, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  DBI::dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

## copy updated db to package data folder + remove working copy
file.copy(addlDBfile, pkgDBfile, overwrite = TRUE)
unlink(wrkngDBfile)
