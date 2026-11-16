## Record one fingerprint per published ClimateNA zip in the tile index, and ship the index.
##
## Reads the zips.csv of the last full audit (04-audit_archives.R, run after any repairs with
## 05-repair_archives.R) and writes its Drive MD5, file count and size into the `archives`
## table. `verified` is the audit time for zips that passed the check, and NULL otherwise.
## Then copies the working index into the package (inst/extdata). The copy on the share
## (`addlDBfile`) is left alone: it holds the per-file checksum tables (millions of rows),
## which the package copy does not.

stop(
  "Superseded by the targets pipeline in data-raw/pipeline/, whose index has the archives ",
  "table. This script copies the pre-pipeline working index over the shipped release index.",
  call. = FALSE
)

source("data-raw/01-ClimateNA_setup.R")

if (!exists("finalZips")) {
  finalZips <- NULL ## path to the zips.csv of the last full audit
}
stopifnot(
  "set `finalZips` to the last full audit's zips.csv" = !is.null(finalZips),
  "`finalZips` not found" = file.exists(finalZips)
)

zips <- utils::read.csv(finalZips, colClasses = c(gid = "character"))
stopifnot(!anyDuplicated(zips$gid), !anyNA(zips$md5_drive))

fingerprints <- data.frame(
  gid = zips$gid,
  type = zips$type,
  zipfile = zips$zipfile,
  md5 = zips$md5_drive,
  n_files = as.integer(zips$n_files),
  size = as.numeric(zips$size),
  verified = ifelse(zips$complete %in% TRUE, zips$checked, NA_real_)
)

db <- sqlite_connect_db(wrkngDBfile)
.archivesCreate(db)
n <- .archivesUpsert(db, fingerprints)
message(n, " fingerprints written; ", sum(is.na(fingerprints$verified)), " zips not verified")
DBI::dbExecute(db, "PRAGMA wal_checkpoint(TRUNCATE)")
DBI::dbDisconnect(db)
stopifnot(!file.exists(paste0(wrkngDBfile, "-wal")) || file.size(paste0(wrkngDBfile, "-wal")) == 0)

sizeBefore <- file.size(pkgDBfile)
file.copy(wrkngDBfile, pkgDBfile, overwrite = TRUE)
message(
  "Shipped index grew by ",
  round((file.size(pkgDBfile) - sizeBefore) / 1024^2, 2),
  " MB (now ",
  round(file.size(pkgDBfile) / 1024^2, 1),
  " MB)"
)
unlink(paste0(wrkngDBfile, c("", "-wal", "-shm")))
