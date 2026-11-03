## Produce ClimateNA historical tiles with the ClimateNAr R package, instead of the Windows
## ClimateNA executable. Sourced by 01-ClimateNA_setup.R; used by 03-ClimateNA_historical.R.
##
## ClimateNAr is not on CRAN: https://climatena.ca/downloads/ClimateNAr.zip (v3.2.0, 2026-04-21).
## Each ClimateNAr() call reads a version file from climatena.ca, so it needs network access.
## Compared with the v7.60 executable on four 2024 tiles: Tmax, Tmin, Tave within 0.1 C (99th
## percentile); monthly PPT within 2 mm; EMT differs (median up to 4.4 C on the coast).

## ClimateNAr() builds its baseline rasters without a CRS, and terra's
## extract(<SpatRaster>, <data.frame>) stops on those because is.lonlat() is NA (terra 1.9.46;
## same code in 1.9-50). Return a copy of ClimateNAr() that sets lon/lat before each extract().
ClimateNAr_fn <- function() {
  stopifnot(packageVersion("ClimateNAr") == "3.2.0")
  src <- deparse(ClimateNAr::ClimateNAr, width.cutoff = 500L)
  stopifnot(sum(grepl("terra::extract(", src, fixed = TRUE)) == 11L)
  env <- new.env(parent = asNamespace("ClimateNAr"))
  env$.extract <- function(x, y, ...) {
    terra::crs(x) <- "EPSG:4326"
    terra::extract(x, y, ...)
  }
  eval(parse(text = gsub("terra::extract(", ".extract(", src, fixed = TRUE)), envir = env)
}

## ClimateNAr() silently returns the 1961-1990 baseline for a period it has no data for,
## so check the years against the data it ships.
ClimateNAr_checkYears <- function(years) {
  f <- file.path(
    system.file("data", "historical", package = "ClimateNAr"),
    paste0("Year_", years, ".ann.rda")
  )
  if (!all(file.exists(f))) {
    stop("ClimateNAr has no historical data for: ", toString(years[!file.exists(f)]))
  }
  invisible(years)
}

## DEM for one tile on the grid of its existing outputs (`template`, any .asc of the tile), built
## the way the 1901-2024 tiles were: GTOPO30 (as in makeClimateDEM()), bilinear, and elevation 0
## in tile cells GTOPO30 does not cover (coast and Arctic edges). Running ClimateNAr at elevation
## 0 in those cells reproduces the existing tiles; geodata::elevation_30s() has the same gaps.
ClimateNAr_tileDEM <- function(template, filename, destinationPath, cachePath) {
  gtopo30N <- Cache(
    prepInputs,
    url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/",
    destinationPath = destinationPath,
    cachePath = cachePath
  )
  tmpl <- terra::rast(template)
  terra::crs(tmpl) <- "EPSG:4326"
  dem <- terra::project(gtopo30N, tmpl, method = "bilinear")
  dem <- terra::ifel(is.na(tmpl), NA, terra::ifel(is.na(dem), 0, dem))
  names(dem) <- "elev0"
  terra::writeRaster(dem, filename, overwrite = TRUE)
}

## Run ClimateNAr for one tile and year, and write .asc files in the executable's layout,
## <ClimateNA_path()>/Year_<year><msy>/<var>.asc, for each of `vars` (a list named by msy, giving
## the variables of that archive; take them from the tile's existing archive).
ClimateNAr_tile <- function(fn, dem, year, outDirs, vars, scratch) {
  tmpl <- terra::rast(dem)
  wd <- file.path(scratch, paste0(tools::file_path_sans_ext(basename(dem)), "_", year))
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  fn(dem, paste0("Year_", year, ".ann"), "YSM", paste0(wd, "/"))

  tifs <- list.files(wd, "[.]tif$", recursive = TRUE, full.names = TRUE)
  names(tifs) <- tools::file_path_sans_ext(basename(tifs))
  stopifnot(all(unlist(vars) %in% names(tifs)))

  for (msy in names(vars)) {
    d <- file.path(outDirs[[msy]], paste0("Year_", year, msy))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    for (v in vars[[msy]]) {
      r <- terra::extend(terra::crop(terra::rast(tifs[[v]]), tmpl, snap = "near"), tmpl, snap = "near")
      stopifnot(terra::compareGeom(r, tmpl))
      f <- file.path(d, paste0(v, ".asc"))
      terra::writeRaster(r, f, NAflag = -9999, gdal = "SIGNIFICANT_DIGITS=6", overwrite = TRUE)
      unlink(c(paste0(f, ".aux.xml"), sub("[.]asc$", ".prj", f)))
    }
  }
  invisible(outDirs)
}
