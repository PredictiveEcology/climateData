## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process historic, projected, and normals data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# setup ---------------------------------------------------------------------------------------

source("data-raw/01-ClimateNA_setup.R")

dbdf <- ClimateNA_sql(wrkngDBfile, "future")
climate_db <- dbdf[["db"]]
future_climate_df <- dbdf[["df"]]
rm(dbdf)

MSYs <- c("MSY", "M", "S", "Y")
GCMs <- available("future")[["gcms"]]
SSPs <- available("future")[["ssps"]]
future_years <- available("future")[["years"]]
future_decades <- (future_years %/% 10 * 10) |> unique() |> as.integer()

runClimateNA <- FALSE ## TRUE
createZips <- FALSE ## TRUE
uploadArchives <- FALSE ## TRUE
reuploadArchives <- FALSE ## TRUE

if (!exists("dem_ff")) {
  dem_ff <- list.files(file.path(ClimateNAdata, "dem"), pattern = "[.]asc$", full.names = TRUE)
  stopifnot(length(dem_ff) > 0)
}

plan("callr", workers = min(length(dem_ff), parallelly::availableCores()))

# use ClimateNA to fetch and process climate data ---------------------------------------------

## TODO: currently, the package is of very low quality,
##       offering little more than a clumsy wrapper around system2()

# if (!"ClimateNAr" %in% row.names(installed.packages())) {
#   dlDir <- file.path("C:/Users", Sys.info()[["user"]], "Downloads")
#   pkgurl <- "https://climatena.ca/downloads/ClimateNAr_1.2.0.zip"
#   pkglcl <- file.path(dlDir, basename(pkgurl))
#   download.file(pkgurl, pkglcl)
#   install.packages(pkglcl, repos = NULL)
# }
#
# library(ClimateNAr)

# get ClimateNA future time series ------------------------------------------------------------

new_rows_future <- future_lapply(dem_ff, function(f) {
  dbdf <- ClimateNA_sql(wrkngDBfile, "future")
  climate_db <- dbdf[["db"]]
  future_climate_df <- dbdf[["df"]]
  rm(dbdf)

  f <- normalizePath(f)

  z <- lapply(GCMs, function(gcm) {
    lapply(SSPs, function(ssp) {
      lapply(MSYs, function(msy) {
        ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "future", msy, gcm, ssp)

        lapply(future_years, function(yr) {
          row <- dplyr::filter(
            future_climate_df,
            gcm == !!gcm & ssp == !!ssp & msy == !!msy & year == !!yr & tileid == !!tileID(f)
          ) |>
            collect()

          if (nrow(row) == 0) {
            if (isTRUE(runClimateNA)) {
              withr::local_dir(ClimateNAdir)
              system2(ClimateNAexe,
                      args = c(
                        paste0("/", msy),
                        paste0("/", gcm, "_ssp", ssp, "@", yr, ".gcm"),
                        paste0("/", f),
                        paste0("/", ClimateNAout)
                      ))
              withr::deferred_run()
            }

            new_row <- data.frame(
              gcm = gcm,
              ssp = ssp,
              msy = msy,
              year = yr,
              tileid = tileID(f),
              created = file.info(ClimateNAout)$mtime,
              stringsAsFactors = FALSE
            )
            # rows_append(future_climate_df, new_row, copy = TRUE, in_place = TRUE)
          } else {
            new_row <- dplyr::mutate(row, created = file.info(ClimateNAout)$mtime)
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

  dbDisconnect(climate_db)

  return(z)
}, future.seed = NULL) |>
  dplyr::bind_rows()

if (!"rowid" %in% colnames(new_rows_future)) {
  new_rows_future <- tibble::rowid_to_column(new_rows_future)
  rows_append(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE)
} else {
  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

dbDisconnect(climate_db)

file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)

# archive tilesets ----------------------------------------------------------------------------

if (createZips) {
  ## future time series
  new_rows_future <- future_lapply(dem_ff, function(f) {
    dbdf <- ClimateNA_sql(wrkngDBfile, "future")
    climate_db <- dbdf[["db"]]
    future_climate_df <- dbdf[["df"]]
    rm(dbdf)

    f <- normalizePath(f)

    z <- lapply(GCMs, function(gcm) {
      lapply(SSPs, function(ssp) {
        lapply(MSYs, function(msy) {
          lapply(future_decades, function(dcd) {
            ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "future", msy, gcm, ssp)
            fzip <- paste0(ClimateNAout, "_", gcm, "_", ssp, "_", msy, "_", dcd, ".zip")

            type <- paste0("future", "_", msy)

            row <- dplyr::filter(
              future_climate_df,
              gcm == !!gcm & ssp == !!ssp & msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tileID(f)
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

    dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  dbDisconnect(climate_db)

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
    dbdf <- ClimateNA_sql(wrkngDBfile, "future")
    climate_db <- dbdf[["db"]]
    future_climate_df <- dbdf[["df"]]
    rm(dbdf)

    f <- normalizePath(f)

    z <- lapply(GCMs, function(gcm) {
      lapply(SSPs, function(ssp) {
        lapply(MSYs, function(msy) {
          ## re-auth to avoid curl::curl_fetch_memory() "Error in the HTTP2 framing layer"
          googledrive::drive_auth(email = userEmail, cache = oauthCachePath)

          gid <- dplyr::filter(gids_future, gcm == !!gcm, ssp == !!ssp)[["id"]]
          drivefiles <- googledrive::drive_ls(gid)

          lapply(future_decades, function(dcd) {
            ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "future", msy, gcm, ssp)
            fzip <- paste0(ClimateNAout, "_", gcm, "_", ssp, "_", msy, "_", dcd, ".zip")

            type <- paste0("future", "_", msy)

            row <- dplyr::filter(
              future_climate_df,
              gcm == !!gcm & ssp == !!ssp & msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tileID(f)
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

    dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(future_climate_df, new_rows_future, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  dbDisconnect(climate_db)

  file.copy(wrkngDBfile, addlDBfile, overwrite = TRUE)
}

## copy updated db to module data folder
file.copy(addlDBfile, pkgDBfile, overwrite = TRUE)
