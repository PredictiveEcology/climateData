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
climatena_dir_path <- local$climatena_dir
dem_dir_path <- local$dem_dir
out_root_path <- local$out_root
container_uid <- local$uid
container_gid <- local$gid
tile_subset <- local$tiles
dry_run <- local$dry_run
## replace a release folder's file in place when its rebuilt zip differs; otherwise that stops
replace_changed <- isTRUE(local$replace_changed)
run_timeout <- local$run_timeout %||% 3600 # seconds per ClimateNA run
## the previous release's index, whose identical zips are reused rather than uploaded again
## (relative, so it is the same path in any checkout)
previous_index_path <- local$previous_index %||% "inst/extdata/ClimateNA_tiles.sqlite"
## node-local directory for ClimateNA's output before it is copied to out_root (an environment
## variable, so it is no target's dependency; local crew workers inherit it)
if (!is.null(local$work_dir)) {
  Sys.setenv(CLIMATENA_WORK_DIR = local$work_dir)
}
drive_email <- local$drive_email # the Google account whose cached token uploads use
if (!is.null(drive_email)) {
  options(gargle_oauth_email = drive_email)
}

devtools::load_all(".", quiet = TRUE) # the main process uses the checkout too

## The pipeline uses at most half of each node's cores (.cappedWorkers()). Each ClimateNA run is
## limited to one CPU, so on this node: ClimateNA workers + upload workers + this process.
if (!is.null(local$controller)) {
  stop(
    "_local.R: set `workers` (or `ssh_nodes`) instead of `controller`; ",
    "the pipeline sizes its workers to at most half of each node's cores"
  )
}
upload_workers <- .cappedWorkers(local$upload_workers %||% 8L, parallelly::availableCores(), 1L)
climatena_controller <- if (is.null(local$ssh_nodes)) {
  crew::crew_controller_local(
    workers = .cappedWorkers(
      local$workers %||% 4L,
      parallelly::availableCores(),
      upload_workers + 1L
    )
  )
} else {
  ## other nodes (FOR-CAST/crew.ssh) run ClimateNA only
  nodes <- local$ssh_nodes
  for (n in names(nodes)) {
    cores <- as.integer(system2("ssh", c(n, "nproc"), stdout = TRUE))
    nodes[[n]] <- .cappedWorkers(nodes[[n]], cores, 0L, where = n)
  }
  crew.ssh::crew_controller_ssh(nodes = nodes, projdir = local$projdir)
}

tar_option_set(
  ## nothing attached on workers: the default (the main process's attached packages) would
  ## include the load_all()ed climateData, which isn't installed; pipeline_load() loads it
  packages = character(0),
  ## ClimateNA runs go to climatena_controller; uploads to a few local workers on this node,
  ## where the Drive token is
  controller = crew::crew_controller_group(
    climatena_controller,
    crew::crew_controller_local(name = "uploads", workers = upload_workers)
  ),
  deployment = "main", # small targets stay on the main process
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

  ## ClimateNA point-mode values for the batch's empty land cells (fill_points), one CSV per
  ## period beside its raw zip; none for a tile with nothing to fill
  tar_target(
    fills,
    fill_batch(
      specs,
      batch,
      fill_points,
      out_root,
      pipeline_point_runner(
        image,
        climatena_exe,
        climatena_dir,
        container_uid,
        container_gid,
        run_timeout
      )
    ),
    pattern = map(specs, batch),
    format = "file",
    deployment = "worker"
  ),

  ## the batch's MSY, M, Y and S zips (Y for normals), masked and filled, each checked
  tar_target(
    zips,
    zip_batch(specs, batch, out_root, tile_masks, fills),
    pattern = map(specs, batch, fills),
    format = "file",
    deployment = "worker"
  ),

  ## each zip as an earlier release's identical file, or a file in the release's Drive folder
  ## (replaced in place when the zip changed, with replace_changed; otherwise that stops)
  tar_target(
    uploads,
    upload_zips(
      zips,
      specs,
      release_folders,
      dry_run,
      previous_lookup,
      drive_email,
      replace_changed
    ),
    pattern = map(zips, specs),
    deployment = "worker",
    resources = tar_resources(crew = tar_resources_crew(controller = "uploads"))
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
  tar_target(climatena_dir, climatena_dir_path),
  tar_target(dem_dir, dem_dir_path),
  tar_target(out_root, out_root_path),
  ## the DEM folder's tiles, listed on every run: adding or removing a DEM file changes neither
  ## dem_dir nor the tile subset (an unchanged list rebuilds nothing)
  tar_target(tiles, pipeline_tiles(dem_dir, tile_subset), cue = tar_cue(mode = "always")),
  ## the DEM files themselves, so what is built from them follows their contents. This and the
  ## other release-wide inputs stop the run when they fail: under error = "null" one failure
  ## would become an error in every branch
  tar_target(dem_files, pipeline_dem_files(dem_dir, tiles), format = "file", error = "stop"),
  ## the cells each tile's zips set to NODATA (no DEM elevation, centre outside Canada; #40),
  ## and its land cells
  tar_target(canada, pipeline_canada(out_root), format = "file"),
  tar_target(tile_masks, pipeline_tile_masks(canada, dem_files), error = "stop"),
  tar_target(previous_index, previous_index_path, format = "file"),

  mapped,

  ## all types together, for the Drive folders and the index
  tar_combine(specs_all, mapped[["specs"]], command = c(!!!.x), iteration = "list"),

  ## land cells ClimateNA leaves empty, and the nearest land cell it covers for each (checked
  ## with a point run), from each class's reference period in the raw outputs
  tar_target(fill_reference, pipeline_fill_reference(cfg)),
  tar_target(fill_max_km, cfg[["fill_max_km"]] %||% 50),
  tar_target(
    fill_points,
    pipeline_fill_points(
      manifests,
      tile_masks,
      dem_files,
      fill_reference,
      out_root,
      fill_max_km,
      pipeline_point_runner(
        image,
        climatena_exe,
        climatena_dir,
        container_uid,
        container_gid,
        run_timeout
      )
    ),
    deployment = "worker",
    error = "stop"
  ),
  tar_target(
    release_folders,
    pipeline_release_folders(specs_all, release_id, drive_parent, dry_run, out_root)
  ),
  tar_target(previous_lookup, pipeline_previous_lookup(previous_index, specs_all, out_root)),
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
      dem_resolution,
      fill_points
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
