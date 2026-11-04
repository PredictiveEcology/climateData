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
run_timeout <- local$run_timeout %||% 3600 # seconds per ClimateNA run

devtools::load_all(pkg_dir_path, quiet = TRUE) # the main process uses the checkout too

tar_option_set(
  ## nothing attached on workers: the default (the main process's attached packages) would
  ## include the load_all()ed climateData, which isn't installed; pipeline_load() loads it
  packages = character(0),
  controller = local$controller,
  deployment = "main", # small targets stay on the main process; ClimateNA runs go to workers
  error = "null",
  storage = "worker",
  retrieval = "worker"
)
tar_source("data-raw/pipeline/R")

## one set of targets per archive type
mapped <- tar_map(
  values = data.frame(type = unlist(cfg$types)),
  names = type,
  unlist = FALSE,
  tar_target(specs, pipeline_specs(type, tiles, cfg), iteration = "list"),
  tar_target(min_version, cfg[["min_version"]][[type]]),

  ## does this ClimateNA release reproduce the stored outputs it would keep? If not, the whole
  ## pipeline stops (error = "stop") before any batch of this type runs
  tar_target(
    compat,
    compat_check(
      type,
      pkg_dir,
      climatena_exe,
      climatena_version,
      min_version,
      image,
      climatena_dir,
      dem_dir,
      out_root,
      container_uid,
      container_gid,
      timeout = run_timeout
    ),
    deployment = "worker",
    error = "stop"
  ),

  ## one branch per batch (a tile's decade, or a tile's normals): run, check, stamp.
  ## Runs on every tar_make() to re-check the outputs on disk; a period already done costs a
  ## file listing, and an unchanged manifest leaves the zips alone.
  tar_target(
    batch,
    {
      compat
      run_batch(
        specs,
        pkg_dir,
        climatena_exe,
        climatena_version,
        image,
        climatena_dir,
        dem_dir,
        out_root,
        min_version,
        container_uid,
        container_gid,
        timeout = run_timeout
      )
    },
    pattern = map(specs),
    format = "file",
    deployment = "worker",
    cue = tar_cue(mode = "always")
  ),

  ## the batch's MSY, M, Y and S zips (Y for normals), each checked
  tar_target(
    zips,
    zip_batch(specs, batch, out_root, pkg_dir),
    pattern = map(specs, batch),
    format = "file",
    deployment = "worker"
  ),

  ## each zip as a new file in the release's Drive folder (main process, where the token is)
  tar_target(
    uploads,
    upload_zips(zips, specs, release_id, drive_parent, dry_run),
    pattern = map(zips, specs)
  )
)

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

  mapped,

  ## all types together, for the index
  tar_combine(specs_all, mapped[["specs"]], command = c(!!!.x), iteration = "list"),
  tar_combine(uploads_all, mapped[["uploads"]], command = rbind(!!!.x)),
  tar_combine(manifests, mapped[["batch"]], command = c(!!!.x)),

  ## the release's ClimateNA_tiles.sqlite
  tar_target(
    index,
    build_index(
      uploads_all,
      specs_all,
      manifests,
      out_root,
      release_id,
      climatena_version,
      dem_resolution
    ),
    format = "file"
  ),

  ## errors, warnings and missing zips, refreshed on every run (empty when all is well)
  tar_target(
    report,
    build_report(index, specs_all, uploads_all, out_root),
    format = "file",
    cue = tar_cue(mode = "always")
  )
)
