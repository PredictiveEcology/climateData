## The pipeline's DEM inputs (data-raw/pipeline/R/specs.R): tile masks are built from the DEM
## files themselves (a `format = "file"` target), so replacing a DEM file rebuilds them.
pipelineEnv <- function() {
  d <- test_path("..", "..", "data-raw", "pipeline", "R")
  skip_if_not(dir.exists(d), "data-raw/pipeline not available")
  e <- new.env()
  for (f in c("specs.R", "run_batch.R")) {
    sys.source(file.path(d, f), envir = e)
  }
  e
}

writeDem <- function(dir, tile, values) {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
  terra::values(r) <- values
  f <- file.path(dir, paste0("can_dem_", tile, ".asc"))
  terra::writeRaster(r, f, NAflag = -9999, overwrite = TRUE)
  f
}

test_that("pipeline_dem_files names one file per tile and stops on a missing one", {
  e <- pipelineEnv()
  d <- withr::local_tempdir()
  writeDem(d, 1, 1:4)
  writeDem(d, 2, 1:4)
  f <- e$pipeline_dem_files(d, c(1L, 2L))
  expect_named(f, c("1", "2"))
  expect_true(all(file.exists(f)))
  expect_error(e$pipeline_dem_files(d, c(1L, 3L)), "no DEM tile.*3")
})

test_that("pipeline_tile_masks names each tile's mask and land cells by its DEM file", {
  e <- pipelineEnv()
  d <- withr::local_tempdir()
  ## tile 2: cell 1 has no elevation; Canada covers the left column, so it is kept (not masked)
  demFiles <- c("1" = writeDem(d, 1, 1:4), "2" = writeDem(d, 2, c(NA, 2, 3, NA)))
  canada <- terra::vect("POLYGON ((0 0, 1 0, 1 2, 0 2, 0 0))", crs = "EPSG:4326")
  canadaFile <- file.path(d, "canada.rds")
  saveRDS(terra::wrap(canada), canadaFile)

  m <- e$pipeline_tile_masks(canadaFile, demFiles)
  expect_named(m, c("1", "2"))
  expect_identical(m[["1"]]$land, 1:4)
  expect_identical(m[["2"]]$land, c(2L, 3L))
  expect_identical(m[["2"]]$cells, 4L)
})
