## The pipeline's target settings (data-raw/pipeline/_targets.R). Needs a checkout with a
## _local.R (gitignored), so it runs only in a pipeline checkout.
test_that("tiles re-lists the DEM folder every run, and the DEM-wide targets stop the run", {
  root <- test_path("..", "..")
  skip_if_not(file.exists(file.path(root, "data-raw", "pipeline", "_local.R")), "no _local.R")
  skip_if_not_installed("targets")
  m <- withr::with_dir(
    root,
    targets::tar_manifest(
      script = "data-raw/pipeline/_targets.R",
      fields = c("name", "cue_mode", "error")
    )
  )
  ## a DEM file added or removed changes neither dem_dir nor the tile subset
  expect_identical(m$cue_mode[m$name == "tiles"], "always")
  ## nothing downstream works without these; under error = "null" a failure would become one
  ## error per branch
  for (n in c("dem_files", "tile_masks", "fill_points")) {
    expect_identical(m$error[m$name == n], "stop", label = n)
  }
})
