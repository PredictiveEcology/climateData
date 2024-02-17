## 1. Create tiles for Canada for use with ClimateNA;
## 2. Download and process historic, projected, and normals data for each tile using ClimateNA;
## 3. Archive and upload each set of tiles for use with canClimateData.

# setup ---------------------------------------------------------------------------------------

source("data-raw/01-ClimateNA_setup.R")

dbdf <- ClimateNA_sql(tempDBfile, "historic")
climate_db <- dbdf[["db"]]
climate_historic_df <- dbdf[["df"]]
rm(dbdf)

MSYs <- c("MSY", "M", "S", "Y")

historic_years <- 1901L:2022L
# historic_years <- 1901L:1990L
# historic_years <- 1991L:2022L
historic_decades <- (historic_years %/% 10 * 10) |> unique() |> as.integer()
period_ann <- paste0("Year_", historic_years, ".ann")

runClimateNA <- FALSE ## TRUE
createZips <- FALSE ## TRUE
uploadArchives <- FALSE ## TRUE

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

# get ClimateNA historic time series ----------------------------------------------------------

new_rows_historic <- future_lapply(dem_ff, function(f) {
  dbdf <- ClimateNA_sql(tempDBfile, "historic")
  climate_db <- dbdf[["db"]]
  climate_historic_df <- dbdf[["df"]]
  rm(dbdf)

  f <- normalizePath(f)

  z <- lapply(MSYs, function(msy) {
    lapply(period_ann, function(ann) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "historic", msy)

      yr <- substr(ann, 6, 9)

      row <- dplyr::filter(
        climate_historic_df,
        msy == !!msy & year == !!yr & tileid == !!tileID(f)
      ) |>
        collect()

      if (nrow(row) == 0) {
        if (isTRUE(runClimateNA)) {
          withr::local_dir(ClimateNAdir)
          system2(ClimateNAexe,
                  args = c(
                    paste0("/", msy),
                    paste0("/", ann),
                    paste0("/", f),
                    paste0("/", ClimateNAout)
                  ))
          withr::deferred_run()
        }

        new_row <- data.frame(
          ## rowid will be filled automatically
          msy = msy,
          year = yr,
          tileid = tileID(f),
          created = file.info(ClimateNAout)$mtime,
          stringsAsFactors = FALSE
        )
        # rows_append(climate_historic_df, new_row, copy = TRUE, in_place = TRUE)
      } else {
        new_row <- dplyr::mutate(row, created = file.info(ClimateNAout)$mtime)
      }

      return(new_row)
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  dbDisconnect(climate_db)

  return(z)
}, future.seed = NULL) |>
  dplyr::bind_rows()

if (!"rowid" %in% colnames(new_rows_historic)) {
  new_rows_historic <- tibble::rowid_to_column(new_rows_historic)
  rows_append(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE)
} else {
  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")
}

dbDisconnect(climate_db)

file.copy(tempDBfile, primaryDBfile, overwrite = TRUE)

# archive tilesets ----------------------------------------------------------------------------

if (createZips) {
  ## historic time series
  new_rows_historic <- future_lapply(dem_ff, function(f) {
    dbdf <- ClimateNA_sql(tempDBfile, "historic")
    climate_db <- dbdf[["db"]]
    climate_historic_df <- dbdf[["df"]]
    rm(dbdf)

    f <- normalizePath(f)

    z <- lapply(MSYs, function(msy) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "historic", msy)

      lapply(historic_decades, function(dcd) {
        fzip <- paste0(ClimateNAout, "_", msy, "_", dcd, ".zip")

        row <- dplyr::filter(
          climate_historic_df,
          msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tileID(f) ## zip each decade
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

    dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  dbDisconnect(climate_db)

  file.copy(tempDBfile, primaryDBfile, overwrite = TRUE)
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

    dbdf <- ClimateNA_sql(tempDBfile, "historic")
    climate_db <- dbdf[["db"]]
    climate_historic_df <- dbdf[["df"]]
    rm(dbdf)

    f <- normalizePath(f)

    z <- lapply(MSYs, function(msy) {
      ClimateNAout <- ClimateNA_path(ClimateNAdata, tile = tileID(f), type = "historic", msy)

      gid <- googledrive::as_id(gids_historic[[msy]])
      drivefiles <- googledrive::drive_ls(gid)

      lapply(historic_decades, function(dcd) {
        fzip <- paste0(ClimateNAout, "_", msy, "_", dcd, ".zip")

        row <- dplyr::filter(
          climate_historic_df,
          msy == !!msy & year %in% !!(dcd + 0:9) & tileid == !!tileID(f) ## zip each decade
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

    dbDisconnect(climate_db)

    return(z)
  }) |>
    dplyr::bind_rows()

  rows_update(climate_historic_df, new_rows_historic, copy = TRUE, in_place = TRUE, unmatched = "ignore")

  dbDisconnect(climate_db)

  file.copy(tempDBfile, primaryDBfile, overwrite = TRUE)
}

## copy updated db to module data folder
file.copy(primaryDBfile, pkgDBfile, overwrite = TRUE)
