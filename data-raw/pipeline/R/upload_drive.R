## Publishing a batch's zips to Google Drive, as files in the release's folder
## <drive_parent>/<release_id>/<type>[/<gcm>_ssp<ssp>]/<subset>/, or as an earlier release's file
## when the zip is identical. This is the only Drive-specific code: another store (e.g. S3) would
## be an upload_<store>.R with the same signature and result.
##
## Uploads run on the main node (where the maintainer's OAuth token is cached), in parallel on the
## "uploads" crew controller; `drive_email` in _local.R tells googledrive which token to use. The
## release's folders are created first, on the main process, so parallel uploads never race to
## create the same folder.

## Drive folder ids looked up or created in this session, keyed by path below the parent.
pipeline_drive_folders <- new.env(parent = emptyenv())

## The id of the folder <parentId>/<segments[1]>/<segments[2]>/..., creating any that are missing.
pipeline_drive_folder <- function(parentId, segments) {
  id <- parentId
  for (i in seq_along(segments)) {
    key <- paste(c(parentId, segments[seq_len(i)]), collapse = "/")
    if (is.null(pipeline_drive_folders[[key]])) {
      found <- googledrive::drive_ls(googledrive::as_id(id), type = "folder")
      found <- found[found$name == segments[i], ]
      if (nrow(found) > 1) {
        stop("several Drive folders named ", key)
      }
      pipeline_drive_folders[[key]] <- if (nrow(found) == 1) {
        as.character(found$id)
      } else {
        googledrive::with_drive_quiet(googledrive::drive_mkdir(
          segments[i],
          path = googledrive::as_id(id)
        ))$id |>
          as.character()
      }
    }
    id <- pipeline_drive_folders[[key]]
  }
  id
}

## Create the release's Drive folder for every zip directory it can produce (e.g.
## "historical/MSY"), and write their ids (NA in a dry run), named by the directory below the
## release folder, to <outRoot>/drive_folders.rds. Returns only the map's path, the release and
## the dry-run flag, so adding folders (a new GCM or SSP) changes nothing the existing upload
## branches depend on.
pipeline_release_folders <- function(specs, releaseId, driveParent, dryRun, outRoot) {
  dirs <- unique(dirname(unlist(lapply(specs, pipeline_zip_relpaths))))
  ids <- if (isTRUE(dryRun)) {
    rep(NA_character_, length(dirs))
  } else {
    vapply(
      dirs,
      \(d) pipeline_drive_folder(driveParent, c(releaseId, strsplit(d, "/")[[1]])),
      character(1)
    )
  }
  path <- file.path(outRoot, "drive_folders.rds")
  saveRDS(stats::setNames(ids, dirs), path)
  list(path = path, release_id = releaseId, dry_run = dryRun)
}

## Every zip path below the release folder that a batch can produce.
pipeline_zip_relpaths <- function(spec) {
  vapply(pipeline_subsets(spec$type), \(s) pipeline_zip_relpath(spec, s), character(1))
}

## The previous release's archives rows that this release could reuse, written once to a small
## lookup database (indexed, rollback-journal mode, so upload workers can open it read-only and
## query only their own zips). Returns the path and a hash of the rows: uploads then depend on
## what the rows are, not on the database file's bytes.
pipeline_previous_lookup <- function(index, specs, outRoot) {
  prev <- pipeline_previous_archives(index)
  prev <- prev[prev$zipfile %in% unlist(lapply(specs, pipeline_zip_relpaths)), , drop = FALSE]
  prev <- prev[order(prev$zipfile), , drop = FALSE]
  path <- file.path(outRoot, "previous_archives.sqlite")
  tmp <- paste0(path, ".tmp")
  unlink(tmp)
  db <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbExecute(db, "CREATE TABLE prev (zipfile TEXT, gid TEXT, md5 TEXT)")
  DBI::dbAppendTable(db, "prev", prev[c("zipfile", "gid", "md5")])
  DBI::dbExecute(db, "CREATE INDEX prev_zipfile ON prev (zipfile)")
  DBI::dbDisconnect(db)
  file.rename(tmp, path)
  list(path = path, hash = digest::digest(prev, algo = "xxhash64"))
}

## The previous release's rows for one batch's zips, from pipeline_previous_lookup().
pipeline_previous_rows <- function(lookup, spec) {
  db <- DBI::dbConnect(RSQLite::SQLite(), lookup$path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(db))
  DBI::dbGetQuery(
    db,
    "SELECT gid, zipfile, md5 FROM prev WHERE zipfile = ?",
    params = list(unname(pipeline_zip_relpaths(spec)))
  )
}

## Drive's createdTime (RFC 3339, UTC) as epoch seconds.
pipeline_drive_time <- function(resource) {
  as.numeric(as.POSIXct(resource$createdTime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
}

## The `archives` rows (gid, zipfile, md5) of an earlier release's index, for reusing its Drive
## files; none when the file is missing or has no archives table. Read from a copy: the shipped
## index is in WAL mode, which a read-only connection can't open in place.
pipeline_previous_archives <- function(index) {
  none <- data.frame(gid = character(0), zipfile = character(0), md5 = character(0))
  if (length(index) == 0 || !file.exists(index)) {
    return(none)
  }
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(paste0(tmp, c("", "-wal", "-shm"))), add = TRUE)
  for (ext in c("", "-wal", "-shm")) {
    if (file.exists(paste0(index, ext))) {
      file.copy(paste0(index, ext), paste0(tmp, ext))
    }
  }
  db <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(db), add = TRUE, after = FALSE)
  if (!DBI::dbExistsTable(db, "archives")) {
    return(none)
  }
  x <- DBI::dbGetQuery(db, "SELECT gid, zipfile, md5 FROM archives WHERE gid IS NOT NULL")
  if (nrow(x) == 0) none else x
}

## An earlier release's Drive file for this zip: same path below the release folder and same MD5,
## and still on Drive (not trashed) with that MD5. NULL when there is none.
pipeline_reusable_file <- function(previous, rel, md5) {
  prev <- previous[previous$zipfile == rel & previous$md5 == md5, , drop = FALSE]
  if (nrow(prev) == 0) {
    return(NULL)
  }
  file <- tryCatch(googledrive::drive_get(googledrive::as_id(prev$gid[1])), error = \(e) NULL)
  r <- if (!is.null(file) && nrow(file) == 1) file$drive_resource[[1]]
  if (is.null(r) || isTRUE(r$trashed) || !identical(r$md5Checksum, md5)) {
    return(NULL)
  }
  file
}

## Upload one batch's zips. Each zip is, in order of preference:
##   - "reused": an earlier release (`lookup`, from pipeline_previous_lookup()) has the identical
##     zip on Drive;
##   - "found": the release folder already has a file of that name and MD5 (an interrupted run);
##   - "uploaded" as a new file. A same-named file with another MD5 is left alone, with a warning.
## With dryRun nothing on Drive is read or written: `source` is "reused" (gid from `lookup`, not
## checked) or "dry run" (gid NA). `folders` is pipeline_release_folders(). Returns one row per
## zip: subset, gid, md5, zipfile (path below the release folder), uploaded (Drive's createdTime,
## epoch seconds; NA in a dry run) and source.
upload_zips <- function(zips, spec, folders, dryRun, lookup, driveEmail = NULL) {
  if (!is.null(driveEmail)) {
    options(gargle_oauth_email = driveEmail) # a worker process doesn't see _local.R's options
  }
  previous <- if (length(zips) > 0) pipeline_previous_rows(lookup, spec)
  rows <- lapply(zips, function(zip) {
    subset <- basename(dirname(zip))
    rel <- pipeline_zip_relpath(spec, subset)
    out <- data.frame(
      subset = subset,
      gid = NA_character_,
      md5 = unname(tools::md5sum(zip)),
      zipfile = rel,
      uploaded = NA_real_,
      source = "dry run"
    )
    if (isTRUE(dryRun)) {
      prev <- previous$gid[previous$zipfile == rel & previous$md5 == out$md5]
      if (length(prev) > 0) {
        out$gid <- prev[1]
        out$source <- "reused"
      }
      return(out)
    }

    file <- pipeline_reusable_file(previous, rel, out$md5)
    out$source <- "reused"
    if (is.null(file)) {
      folder <- unname(readRDS(folders$path)[dirname(rel)])
      if (length(folder) != 1 || is.na(folder)) {
        stop("no release folder for ", dirname(rel))
      }
      there <- googledrive::drive_ls(
        googledrive::as_id(folder),
        q = sprintf("name = '%s'", basename(zip))
      )
      md5s <- vapply(there$drive_resource, \(r) r$md5Checksum %||% NA_character_, character(1))
      same <- which(md5s == out$md5)
      out$source <- "found"
      if (length(same) > 0) {
        file <- there[same[1], ]
      } else {
        if (nrow(there) > 0) {
          warning(
            "Drive already has a different ",
            basename(zip),
            " in ",
            dirname(rel),
            " (",
            paste(there$id, collapse = ", "),
            "); uploading a new file"
          )
        }
        file <- googledrive::with_drive_quiet(googledrive::drive_upload(
          zip,
          path = googledrive::as_id(folder),
          name = basename(zip),
          overwrite = NA
        ))
        if (!identical(file$drive_resource[[1]]$md5Checksum, out$md5)) {
          stop("MD5 mismatch after uploading ", rel, " (Drive file ", file$id, ")")
        }
        out$source <- "uploaded"
      }
    }
    out$gid <- as.character(file$id)
    out$uploaded <- pipeline_drive_time(file$drive_resource[[1]])
    out
  })
  if (length(rows) == 0) {
    ## an errored zips branch (error = "null")
    return(data.frame(
      subset = character(0),
      gid = character(0),
      md5 = character(0),
      zipfile = character(0),
      uploaded = numeric(0),
      source = character(0)
    ))
  }
  do.call(rbind, rows)
}
