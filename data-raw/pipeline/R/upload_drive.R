## Publishing a batch's zips to Google Drive, as new files in the release's folder
## <drive_parent>/<release_id>/<type>[/<gcm>_ssp<ssp>]/<subset>/. This is the only Drive-specific
## code: another store (e.g. S3) would be an upload_<store>.R with the same signature and result.
##
## Runs on the main process (where the maintainer's OAuth token is): set
## options(gargle_oauth_email = ...) in _local.R so googledrive finds the cached token.

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

## Drive's createdTime (RFC 3339, UTC) as epoch seconds.
pipeline_drive_time <- function(resource) {
  as.numeric(as.POSIXct(resource$createdTime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
}

## Upload one batch's zips. A file of the same name and MD5 already in the folder (from an
## interrupted run) is reused; a same-named file with another MD5 is left alone, with a warning,
## and the zip is uploaded as a new file. With dryRun, nothing is uploaded and gid is NA.
## Returns one row per zip: subset, gid, md5, zipfile (path below the release folder) and
## uploaded (Drive's createdTime, epoch seconds; NA when not uploaded).
upload_zips <- function(zips, spec, releaseId, driveParent, dryRun) {
  rows <- lapply(zips, function(zip) {
    subset <- basename(dirname(zip))
    rel <- pipeline_zip_relpath(spec, subset)
    out <- data.frame(
      subset = subset,
      gid = NA_character_,
      md5 = unname(tools::md5sum(zip)),
      zipfile = rel,
      uploaded = NA_real_
    )
    if (isTRUE(dryRun)) {
      return(out)
    }

    folder <- pipeline_drive_folder(driveParent, c(releaseId, strsplit(dirname(rel), "/")[[1]]))
    there <- googledrive::drive_ls(
      googledrive::as_id(folder),
      q = sprintf("name = '%s'", basename(zip))
    )
    md5s <- vapply(there$drive_resource, \(r) r$md5Checksum %||% NA_character_, character(1))
    same <- which(md5s == out$md5)
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
      uploaded = numeric(0)
    ))
  }
  do.call(rbind, rows)
}
