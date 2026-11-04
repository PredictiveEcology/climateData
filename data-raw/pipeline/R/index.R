## The release's tile index and the run report.

## Index rows for one batch: one per (subset zip, year or period), for every subset zip that was
## uploaded. NULL when the batch has no manifest (it failed).
pipeline_index_rows <- function(spec, uploads, manifestById, outRoot) {
  manifest <- manifestById[[spec$id]]
  if (is.null(manifest)) {
    return(NULL)
  }
  m <- utils::read.delim(manifest, colClasses = "character")
  folders <- .periodFolder(spec$type, spec$periods, spec$msy, spec$gcm, spec$ssp)
  created <- as.numeric(m$time[match(folders, m$folder)])
  version <- m$climatena_version[match(folders, m$folder)]
  normals <- spec$type %in% c("historical_normals", "future_normals")

  rows <- lapply(pipeline_subsets(spec$type), function(subset) {
    rel <- pipeline_zip_relpath(spec, subset)
    up <- uploads[uploads$zipfile == rel, , drop = FALSE]
    if (nrow(up) == 0) {
      return(NULL)
    }
    df <- data.frame(
      gcm = spec$gcm,
      ssp = spec$ssp,
      msy = subset,
      period = as.character(spec$periods),
      year = suppressWarnings(as.integer(spec$periods)),
      tileid = as.integer(spec$tile),
      created = unname(created),
      zipfile = rel,
      archived = as.numeric(file.mtime(pipeline_zip_path(outRoot, spec, subset))),
      gid = up$gid[1],
      uploaded = up$uploaded[1],
      climatena_version = version
    )
    drop <- c(if (is.na(spec$gcm)) c("gcm", "ssp"), if (normals) "year" else "period")
    df[setdiff(names(df), drop)]
  })
  do.call(rbind, rows)
}

## Build <outRoot>/ClimateNA_tiles.sqlite for the release, from scratch: the four tile tables (via
## ClimateNA_sql(), with each row's ClimateNA version), the archives fingerprints and the release
## table. Written to a temporary name and renamed, and left in rollback-journal mode with no
## -wal/-shm files. `uploads`, `specs` and `manifests` are all types' targets combined.
build_index <- function(
  uploads,
  specs,
  manifests,
  outRoot,
  releaseId,
  climatenaVersion,
  demResolution
) {
  manifestById <- as.list(manifests)
  names(manifestById) <- sub("[.]tsv$", "", basename(manifests))
  path <- file.path(outRoot, "ClimateNA_tiles.sqlite")
  tmp <- tempfile("index-", tmpdir = outRoot, fileext = ".sqlite")
  on.exit(unlink(paste0(tmp, c("", "-wal", "-shm"))), add = TRUE)

  types <- vapply(specs, `[[`, character(1), "type")
  for (type in .batchTypes) {
    rows <- lapply(specs[types == type], pipeline_index_rows, uploads, manifestById, outRoot)
    rows <- do.call(rbind, rows)
    con <- ClimateNA_sql(tmp, type) # creates the table even when the release has no such rows
    if (!is.null(rows) && nrow(rows) > 0) {
      DBI::dbAppendTable(con$db, type, rows)
    }
    DBI::dbDisconnect(con$db)
  }

  db <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  .releaseCreate(db)
  .releaseWrite(
    db,
    data.frame(
      release_id = releaseId,
      climatena_version = climatenaVersion,
      dem_resolution = demResolution,
      created = as.numeric(Sys.time())
    )
  )
  .archivesCreate(db)
  if (nrow(uploads) > 0) {
    zips <- file.path(outRoot, "zips", uploads$zipfile)
    .archivesUpsert(
      db,
      data.frame(
        gid = uploads$gid,
        type = sub("/.*", "", uploads$zipfile),
        zipfile = uploads$zipfile,
        md5 = uploads$md5,
        n_files = vapply(zips, \(z) nrow(.zipListing(z)), integer(1)),
        size = as.numeric(file.size(zips)),
        verified = uploads$uploaded # MD5 checked against Drive at upload
      )
    )
  }
  DBI::dbExecute(db, "PRAGMA wal_checkpoint(TRUNCATE)")
  DBI::dbExecute(db, "PRAGMA journal_mode = DELETE")
  DBI::dbDisconnect(db)

  stopifnot(!file.exists(paste0(tmp, c("-wal", "-shm"))))
  file.rename(tmp, path)
  path
}

## Write <outRoot>/report.csv: every error and warning targets has recorded, and every subset zip
## that is missing from the uploads (its batch, zip or upload failed). Empty when all is well.
build_report <- function(index, specs, uploads, outRoot) {
  meta <- targets::tar_meta(fields = c("error", "warnings"), store = targets::tar_path_store())
  problems <- rbind(
    data.frame(target = meta$name, kind = "error", message = meta$error)[!is.na(meta$error), ],
    data.frame(target = meta$name, kind = "warning", message = meta$warnings)[
      !is.na(meta$warnings),
    ]
  )
  missing <- lapply(specs, function(spec) {
    rel <- vapply(pipeline_subsets(spec$type), \(s) pipeline_zip_relpath(spec, s), character(1))
    rel <- setdiff(rel, uploads$zipfile)
    if (length(rel) == 0) {
      return(NULL)
    }
    data.frame(target = spec$id, kind = "incomplete", message = paste("not uploaded:", rel))
  })
  problems <- do.call(rbind, c(list(problems), missing))
  out <- file.path(outRoot, "report.csv")
  utils::write.csv(problems, out, row.names = FALSE)
  out
}
