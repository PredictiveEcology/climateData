source("data-raw/01-ClimateNA_setup.R")

# create tiles --------------------------------------------------------------------------------

targetCRS <- "epsg:4326" ## ClimateNA needs lat/lon, and the DEM already uses this

canada <- geodata::gadm(country = "CAN", level = 0, path = extdataPath, version = "4.1", resolution = 1) |>
  st_as_sf() |>
  st_transform(targetCRS) |>
  nngeo::st_remove_holes()

tiles0 <- st_make_grid(canada, n = c(10, 10))

idx <- st_intersects(tiles0, canada) |>
  lapply(any) |>
  unlist() |>
  which()

tiles <- tiles0[idx] |>
  st_as_sf() |>
  dplyr::mutate(id = dplyr::row_number(), .before = x) |>
  dplyr::rename(geom = x)
st_write(tiles, file.path(extdataPath, "tiles.gpkg"), append = FALSE) ## can't write sqlite file to network drive
file.copy(from = file.path(extdataPath, "tiles.gpkg"),
          to = file.path(ClimateNAdata, "tiles.gpkg"),
          overwrite = TRUE) ## write locally; copy to share.

if (interactive()) {
  plot(st_geometry(tiles), col = "lightgrey")
  plot(st_geometry(canada), add = TRUE)
}

arcSecRes <- c(60, 60) ## .asc format needs these to be the same
gtopo30N <- prepInputs(
  url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/",
  destinationPath = extdataPath
) |>
  project(targetCRS, res = arcSecRes/60/60) |>
  crop(canada) |>
  mask(canada) |>
  trim()

if (interactive()) {
  plot(gtopo30N, legend = FALSE)
  plot(st_geometry(tiles), add = TRUE, border = "blue", lwd = 2)
  st_centroid(tiles) |>
    st_coordinates() |>
    text(col = "blue")
}

dem_dir <- file.path(ClimateNAdata, "dem")
if (!dir.exists(dem_dir)) {
  dir.create(dem_dir, recursive = TRUE)
}
dem_ff <- makeTiles(
  x = gtopo30N,
  y = vect(tiles),
  filename = file.path(dem_dir, "can_dem_.asc"),
  na.rm = TRUE
)

## NOTE: rewrite line endings in asc files for windows
lapply(dem_ff, function(f) {
  rewrite_asc(f)
})

if (interactive()) {
  ## visually check that the raster ids match the polygon ids for the tiles
  dem <- vrt(dem_ff)
  plot(dem, legend = FALSE)
  plot(st_geometry(tiles), add = TRUE)

  lapply(dem_ff, function(f) {
    id <-  tileID(f)
    r <- rast(f)
    text(cbind(xmin(r) + (xmax(r) - xmin(r)) / 2, ymin(r) + (ymax(r) - ymin(r)) / 2), id)

    return(id)
  })

  rm(vrt)
}
