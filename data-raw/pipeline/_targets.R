## ClimateNA data pipeline: builds one data release of climateData's tile archives.
##
## Run from the package root, on a compute node:
##   targets::tar_make(script = "data-raw/pipeline/_targets.R",
##                     store = file.path(<out_root>, "_targets"))
## Settings: release.yml (committed) and _local.R (per machine; see _local.R.example).
##
## Dependencies are kept fine-grained on purpose: no target command uses the whole `local`
## list or the crew controller, and per-field settings are their own targets, so editing the
## release (new years, GCMs or SSPs) rebuilds only the branches whose own inputs changed.

library(targets)
library(tarchetypes)

source("data-raw/pipeline/_local.R") # defines `local`
cfg <- yaml::read_yaml(
  if (is.null(local$release_file)) "data-raw/pipeline/release.yml" else local$release_file
)

## scalar settings as plain globals: a target that uses one depends on that value only
pkg_dir_path <- local$pkg_dir
climatena_dir_path <- local$climatena_dir
dem_dir_path <- local$dem_dir
out_root_path <- local$out_root
container_uid <- local$uid
container_gid <- local$gid
tile_subset <- local$tiles
dry_run <- local$dry_run

devtools::load_all(pkg_dir_path, quiet = TRUE) # the main process uses the checkout too

tar_option_set(
  controller = local$controller,
  deployment = "main", # small targets stay on the main process; ClimateNA runs go to workers
  error = "null",
  storage = "worker",
  retrieval = "worker"
)
tar_source("data-raw/pipeline/R")

list(
  ## one target per setting
  ## ([[ ]] rather than $, so no field name reads as a dependency on the target of the same name)
  tar_target(release_id, cfg[["release_id"]]),
  tar_target(climatena_exe, cfg[["climatena_exe"]]),
  tar_target(climatena_version, cfg[["climatena_version"]]),
  tar_target(dem_resolution, cfg[["dem_resolution"]]),
  tar_target(image, cfg[["image"]]),
  tar_target(drive_parent, cfg[["drive_parent"]]),
  tar_target(pkg_dir, pkg_dir_path),
  tar_target(climatena_dir, climatena_dir_path),
  tar_target(dem_dir, dem_dir_path),
  tar_target(out_root, out_root_path),
  tar_target(tiles, pipeline_tiles(dem_dir, tile_subset)),

  ## one set of targets per archive type
  tar_map(
    values = data.frame(type = unlist(cfg$types)),
    names = type,
    tar_target(specs, pipeline_specs(type, tiles, cfg), iteration = "list"),
    tar_target(min_version, cfg[["min_version"]][[type]])
  )
)
