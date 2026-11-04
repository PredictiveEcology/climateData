## Cutting a batch's published zips (MSY, M, Y, S; Y for normals) from its MSY outputs.

pipeline_subsets <- function(type) {
  if (type %in% c("historical_normals", "future_normals")) "Y" else c("MSY", "M", "Y", "S")
}

## Zip path, using the existing names: historical 39_MSY_2020.zip, future
## 39_CanESM5_245_MSY_2020.zip, normals 39_normals_Y.zip / 39_CanESM5_245_normals_Y.zip.
pipeline_zip_path <- function(outRoot, spec, subset) {
  file.path(outRoot, "zips", pipeline_zip_relpath(spec, subset))
}

## The zip's path below <outRoot>/zips (and below the release's Drive folder), e.g.
## historical/MSY/39_MSY_2020.zip; this is the index's `zipfile`.
pipeline_zip_relpath <- function(spec, subset) {
  scen <- if (is.na(spec$gcm)) character(0) else paste0(spec$gcm, "_ssp", spec$ssp)
  normals <- spec$type %in% c("historical_normals", "future_normals")
  name <- paste(
    c(
      spec$tile,
      if (length(scen)) c(spec$gcm, spec$ssp),
      if (normals) "normals",
      subset,
      if (!normals) spec$decade
    ),
    collapse = "_"
  )
  do.call(file.path, as.list(c(spec$type, scen, subset, paste0(name, ".zip"))))
}

## Index-style rows for one zip, for .archiveExpected().
pipeline_zip_rows <- function(spec, subset, zipfile) {
  rows <- data.frame(
    zipfile = zipfile,
    tileid = spec$tile,
    msy = subset,
    gcm = spec$gcm,
    ssp = spec$ssp
  )
  if (spec$type %in% c("historical", "future")) {
    rows <- rows[rep(1, length(spec$periods)), ]
    rows$year <- spec$periods
  } else {
    rows <- rows[rep(1, length(spec$periods)), ]
    rows$period <- spec$periods
  }
  rows
}

## Each subset zip holds each period's folder under the subset's own name (Year_2020M, ...), as
## a ClimateNA /M, /S or /Y run would have written it and as the package's reader expects. Files
## are staged by hard link (copied when linking isn't possible) and every zip is checked.
## No zips when the batch failed (manifest NULL under error = "null"): its folders may hold a
## killed run's truncated files, which the zip check (file names only) would not catch.
zip_batch <- function(spec, manifest, outRoot, pkgDir) {
  if (length(manifest) == 0) {
    return(character(0))
  }
  pipeline_load(pkgDir)
  tileDir <- pipeline_tile_dir(outRoot, spec)
  normals <- spec$type %in% c("historical_normals", "future_normals")
  out <- character(0)
  for (subset in pipeline_subsets(spec$type)) {
    vars <- if (normals) .allowedClimateVars_nrm else get(paste0(".allowedClimateVars_", subset))
    staging <- tempfile("zip-")
    for (p in spec$periods) {
      src <- file.path(tileDir, .periodFolder(spec$type, p, spec$msy, spec$gcm, spec$ssp))
      dst <- file.path(staging, .periodFolder(spec$type, p, subset, spec$gcm, spec$ssp))
      dir.create(dst, recursive = TRUE)
      from <- file.path(src, paste0(vars, ".asc"))
      to <- file.path(dst, paste0(vars, ".asc"))
      linked <- suppressWarnings(file.link(from, to))
      if (!all(linked)) {
        file.copy(from[!linked], to[!linked])
      }
    }
    zip <- pipeline_zip_path(outRoot, spec, subset)
    dir.create(dirname(zip), recursive = TRUE, showWarnings = FALSE)
    unlink(zip)
    archive::archive_write_dir(zip, staging)
    unlink(staging, recursive = TRUE)

    expected <- .archiveExpected(pipeline_zip_rows(spec, subset, basename(zip)), spec$type)
    problems <- .archiveCheck(.zipListing(zip), expected)
    if (!.archiveComplete(problems)) {
      stop(
        "incomplete zip ",
        basename(zip),
        ": ",
        paste(utils::head(problems$item, 5), collapse = ", ")
      )
    }
    out <- c(out, zip)
  }
  out
}
