## Repair of incomplete ClimateNA zips, from a reviewed audit (see 04-audit_archives.R).
##
## Acts only on rows of the reviewed zips.csv whose `repair` is "repairable": the raw ClimateNA
## outputs on the share are complete for every folder the zip should hold. For each one:
##   1. rebuild the whole zip from the raw outputs (raw is the source of truth);
##   2. check the rebuilt zip; skip it if it is not complete;
##   3. check that the Drive file still holds what the audit saw (skip it if it changed), and
##      copy those bytes to a dated backup folder on the share (Drive keeps old versions of
##      binary files for only 30 days);
##   4. replace the Drive file's content in place, keeping its file ID, then the local zip;
##   5. update `archived` and `uploaded` in that zip's index rows (working copy).
##
## With `dryRun <- TRUE` (the default) only steps 1-3's checks run, and nothing on Drive, in
## the index, or in the published data on the share changes. Each zip's row is appended to
## <auditDir>/repairs.csv as soon as it is done, so a run that stops keeps its record; an
## error's status says whether the Drive file had already been replaced.
##
## Users who already downloaded a replaced zip keep their old copy: reproducible::preProcess()
## does not yet download an archive again when its content on Drive changes.

source("data-raw/01-ClimateNA_setup.R")

## set these before sourcing this script
if (!exists("reviewedZips")) {
  reviewedZips <- NULL ## path to the reviewed <auditDir>/zips.csv
}
if (!exists("repairGids")) {
  repairGids <- NULL ## NULL means every repairable row; otherwise a character vector of Drive IDs
}
if (!exists("dryRun")) {
  dryRun <- TRUE
}

stopifnot(
  "set `reviewedZips` to the reviewed zips.csv" = !is.null(reviewedZips),
  "`reviewedZips` not found" = file.exists(reviewedZips)
)

## absolute, because the rebuild runs inside the raw tile directory
reviewedZips <- normalizePath(reviewedZips, mustWork = TRUE)
auditDir <- dirname(reviewedZips)
rebuiltDir <- file.path(auditDir, "rebuilt")
backupDir <- file.path(ClimateNAdata, "_replaced", format(Sys.Date()))
dir.create(rebuiltDir, showWarnings = FALSE)

zips <- utils::read.csv(reviewedZips, colClasses = c(gid = "character", ssp = "character"))
zips <- zips[zips$repair %in% "repairable", , drop = FALSE]
if (!is.null(repairGids)) {
  zips <- zips[zips$gid %in% repairGids, , drop = FALSE]
}
stopifnot(all(zips$type %in% c("historical", "historical_normals", "future")))
message(nrow(zips), " zips to repair", if (dryRun) " (dry run)")

db <- sqlite_connect_db(wrkngDBfile)

md5 <- function(f) if (file.exists(f)) unname(tools::md5sum(f)) else NA_character_

## `progress` lets the caller record how far a zip got if repairOne() stops with an error
repairOne <- function(z, progress) {
  progress$driveReplaced <- FALSE
  progress$out <- out <- data.frame(
    gid = z$gid,
    zipfile = z$zipfile,
    old_md5 = z$md5_drive,
    new_md5 = NA_character_,
    n_files = NA_integer_,
    backup = file.path(backupDir, z$zipfile),
    status = NA_character_,
    time = as.numeric(Sys.time())
  )

  rows <- DBI::dbGetQuery(
    db,
    paste0("SELECT * FROM ", z$type, " WHERE gid = ?"),
    params = list(z$gid)
  )
  if (nrow(rows) == 0) {
    stop("no ", z$type, " index rows for Drive ID ", z$gid, call. = FALSE)
  }
  expected <- .archiveExpected(rows, z$type)
  folders <- unique(dirname(expected))
  tileDir <- file.path(ClimateNAdata, dirname(z$zipfile), z$tileid)

  ## 1. rebuild from raw, leaving out anything not expected (e.g. Thumbs.db); folders made
  ## before ClimateNA v7.60 keep their Rad*/MAR names, as in 04-audit_archives.R
  raw <- .dirListing(tileDir, folders)
  expected <- .archiveExpectedByVersion(raw, expected)$expected
  files <- raw$path[raw$path %in% expected]
  newZip <- file.path(rebuiltDir, basename(z$zipfile))
  unlink(newZip)
  withr::with_dir(tileDir, archive::archive_write_files(newZip, files = files))
  out$new_md5 <- md5(newZip)
  out$n_files <- nrow(.zipListing(newZip))
  progress$out <- out

  ## 2. check the rebuilt zip
  if (!.archiveComplete(.archiveCheck(.zipListing(newZip), expected))) {
    out$status <- "failed_check"
    return(out)
  }

  ## 3. the Drive file must still hold what the audit saw; then back up those bytes
  current <- googledrive::drive_get(googledrive::as_id(z$gid))$drive_resource[[1]]$md5Checksum
  if (!identical(current, z$md5_drive)) {
    out$status <- "drive_changed"
    return(out)
  }
  localZip <- file.path(ClimateNAdata, z$zipfile)
  downloaded <- file.path(auditDir, "downloads", basename(z$zipfile))
  backupSource <- if (identical(md5(localZip), z$md5_drive)) {
    localZip
  } else if (identical(md5(downloaded), z$md5_drive)) {
    downloaded
  } else {
    NA_character_
  }
  if (is.na(backupSource)) {
    out$status <- "no_backup_source"
    return(out)
  }
  if (dryRun) {
    out$status <- "dry_run"
    return(out)
  }
  dir.create(dirname(out$backup), recursive = TRUE, showWarnings = FALSE)
  file.copy(backupSource, out$backup, overwrite = FALSE, copy.date = TRUE)
  stopifnot(identical(md5(out$backup), z$md5_drive))

  ## 4. replace on Drive (same file ID), then the local zip
  res <- googledrive::drive_update(googledrive::as_id(z$gid), media = newZip)
  progress$driveReplaced <- TRUE
  if (!identical(res$drive_resource[[1]]$md5Checksum, out$new_md5)) {
    out$status <- "upload_md5_mismatch"
    return(out)
  }
  file.copy(newZip, localZip, overwrite = TRUE)
  stopifnot(identical(md5(localZip), out$new_md5))

  ## 5. index rows (epoch seconds, as in the existing columns)
  now <- as.numeric(Sys.time())
  DBI::dbExecute(
    db,
    paste0("UPDATE ", z$type, " SET archived = ?, uploaded = ? WHERE gid = ?"),
    params = list(now, now, z$gid)
  )

  out$status <- "replaced"
  out
}

## one row per zip, appended as soon as it is done (earlier runs' rows are kept)
repairsFile <- file.path(auditDir, "repairs.csv")
appendRepair <- function(r) {
  isNew <- !file.exists(repairsFile)
  utils::write.table(
    r,
    repairsFile,
    sep = ",",
    qmethod = "double",
    row.names = FALSE,
    col.names = isNew,
    append = !isNew
  )
}

runRepairs <- function() {
  on.exit(
    {
      DBI::dbExecute(db, "PRAGMA wal_checkpoint(TRUNCATE)")
      DBI::dbDisconnect(db)
    },
    add = TRUE
  )
  vapply(
    seq_len(nrow(zips)),
    function(i) {
      progress <- new.env()
      r <- tryCatch(repairOne(zips[i, ], progress), error = function(e) {
        r <- progress$out
        if (is.null(r)) {
          r <- data.frame(
            gid = zips$gid[i],
            zipfile = zips$zipfile[i],
            old_md5 = zips$md5_drive[i],
            new_md5 = NA_character_,
            n_files = NA_integer_,
            backup = NA_character_,
            status = NA_character_,
            time = as.numeric(Sys.time())
          )
        }
        r$status <- paste0(
          "error",
          if (isTRUE(progress$driveReplaced)) " after the Drive file was replaced",
          ": ",
          conditionMessage(e)
        )
        r
      })
      appendRepair(r)
      message(r$status, ": ", r$zipfile)
      r$status
    },
    character(1)
  )
}

statuses <- runRepairs()
message("Repairs appended to ", repairsFile)
print(table(sub(":.*$", "", statuses)))
