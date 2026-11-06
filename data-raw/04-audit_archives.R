## Audit of the published ClimateNA zips (read-only).
##
## For every zip listed in the tile index, check that it holds every year (or normals period)
## folder the index claims, with every expected variable file present and non-empty. Where the
## local zip on the share is the same file as on Google Drive (same MD5), check the local copy;
## otherwise download the Drive copy and check that. For each incomplete zip, check whether the
## raw ClimateNA outputs on the share are complete enough to rebuild it.
##
## Writes, in `auditDir` on the share:
##   - zips.csv: one row per zip (Drive ID), with its check result and repair class;
##   - report.csv: one row per problem found;
##   - climatena_reruns.csv: raw output folders that are missing or incomplete;
##   - summary.md: counts and the list of repairable zips;
##   - downloads/: Drive copies of incomplete zips, kept as the backup source for the repair.
##
## Nothing is changed on Google Drive, in the index, or on the share outside `auditDir`.
## Repairs are made by 05-repair_archives.R, from a reviewed zips.csv.

## To audit a release built by the pipeline (data-raw/pipeline/), set these before sourcing this
## script, after authenticating googledrive, from the package root:
##   auditIndex <- "<out_root>/ClimateNA_tiles.sqlite"  # the release's index
##   auditZipRoot <- "<out_root>/zips"                  # its local zips, by the index's zipfile
##   auditDriveFolders <- list(release = "<Drive ID of the release folder>")
##   auditDir <- "<out_root>/_audit/<date>"
##   auditTypes <- c("historical", "historical_normals", "future", "future_normals")
## The raw-output check (repair classes) applies only to the pre-pipeline share layout; for a
## release, an incomplete zip is classed "rerun_pipeline".
releaseAudit <- exists("auditIndex")
if (releaseAudit) {
  stopifnot(exists("auditZipRoot"), exists("auditDriveFolders"), exists("auditDir"))
  devtools::load_all(".", quiet = TRUE)
} else {
  source("data-raw/01-ClimateNA_setup.R")
  auditIndex <- wrkngDBfile
  auditZipRoot <- ClimateNAdata
  auditDir <- file.path(ClimateNAdata, "_audit", format(Sys.Date()))
}

## set these before sourcing this script to audit a subset
if (!exists("auditTypes")) {
  auditTypes <- c("historical", "historical_normals", "future")
}
if (!exists("auditTiles")) {
  auditTiles <- NULL ## NULL means all tiles; otherwise an integer vector of tile IDs
}
nCores <- 16L ## parallel workers for reading local zips and raw folders on the share
downloadDir <- file.path(auditDir, "downloads")
dir.create(downloadDir, recursive = TRUE, showWarnings = FALSE)
checked <- as.numeric(Sys.time())

## Drive folders holding the published zips (from the upload sections of the 03-*.R scripts)
driveFolders <- list(
  historical = c(
    MSY = "1r9yZMlXDqEQxBX2aQeDBAkHlzegqZRLL",
    M = "1E3PNXb_ti5a6JSzmBkrRP5UQ-y6Bco7A",
    Y = "1bp6UX5b9nczub0OG41YAhOQUmyerywc6"
  ),
  historical_normals = c(Y = "1FStzgwcLMz4gky2UJtt9z5U8MWR1A94k"),
  future = c(all = "1DH_JF6ZluYsZUpHGRXpXI8Q-DOUOE9AY") ## one subfolder per GCM x SSP
)
if (!releaseAudit) {
  auditDriveFolders <- driveFolders[auditTypes]
}

# index ---------------------------------------------------------------------------------------

db <- DBI::dbConnect(RSQLite::SQLite(), auditIndex, flags = RSQLite::SQLITE_RO)
index <- lapply(auditTypes, function(type) {
  x <- DBI::dbReadTable(db, type)
  if (!is.null(auditTiles)) {
    x <- x[x$tileid %in% auditTiles, , drop = FALSE]
  }
  x
}) |>
  setNames(auditTypes)
DBI::dbDisconnect(db)

zips <- lapply(auditTypes, function(type) {
  x <- index[[type]]
  keys <- intersect(c("gid", "zipfile", "tileid", "gcm", "ssp", "msy"), names(x))
  z <- unique(x[keys])
  z$type <- type
  z
}) |>
  dplyr::bind_rows()
## one zip per Drive ID: rows sharing a gid must agree on zipfile, tile, msy, gcm and ssp
dupGids <- unique(zips$gid[duplicated(zips$gid)])
if (length(dupGids) > 0) {
  stop(
    "Index rows sharing a Drive ID disagree on the zip they describe (zipfile, tileid, ",
    "msy, gcm or ssp). Drive IDs: ",
    paste(dupGids, collapse = ", "),
    call. = FALSE
  )
}

message("Auditing ", nrow(zips), " zips in ", auditDir)

## index rows that available() allows but the index lacks
indexGaps <- lapply(auditTypes, function(type) {
  g <- .archiveIndexGaps(index[[type]], type)
  if (nrow(g) == 0) {
    return(NULL)
  }
  data.frame(
    gid = NA_character_,
    type = type,
    zipfile = NA_character_,
    tileid = g$tileid,
    gcm = g$gcm %||% NA_character_,
    ssp = g$ssp %||% NA_character_,
    msy = g$msy,
    decade_or_period = as.character(g$year %||% g$period),
    problem = "index_gap",
    item = as.character(g$year %||% g$period),
    repair = NA_character_
  )
}) |>
  dplyr::bind_rows()

# Drive metadata ------------------------------------------------------------------------------

driveMeta <- function(d) {
  res <- d$drive_resource
  data.frame(
    id = d$id,
    name = d$name,
    md5 = vapply(res, \(r) r$md5Checksum %||% NA_character_, character(1)),
    size = vapply(res, \(r) as.numeric(r$size %||% NA), numeric(1)),
    trashed = vapply(res, \(r) isTRUE(r$trashed), logical(1))
  )
}

meta <- unlist(auditDriveFolders, use.names = FALSE) |>
  lapply(\(f) googledrive::drive_ls(googledrive::as_id(f), recursive = TRUE)) |>
  dplyr::bind_rows() |>
  driveMeta()
meta <- meta[meta$id %in% zips$gid, , drop = FALSE]

## IDs not found in the folders: look each one up (it may be trashed, moved, or gone)
notListed <- setdiff(zips$gid, meta$id)
if (length(notListed) > 0) {
  message("Looking up ", length(notListed), " Drive IDs not found in the expected folders")
  found <- lapply(notListed, function(id) {
    tryCatch(googledrive::drive_get(googledrive::as_id(id)), error = \(e) NULL)
  })
  found <- found[!vapply(found, is.null, logical(1))]
  if (length(found) > 0) {
    meta <- rbind(meta, driveMeta(dplyr::bind_rows(found)))
  }
}

driveProblems <- .driveProblems(zips, meta)
m <- meta[match(zips$gid, meta$id), , drop = FALSE]
zips$md5_drive <- m$md5
zips$size <- m$size
zips$on_drive <- !zips$gid %in% driveProblems$gid[driveProblems$problem == "drive_missing"]

# check each zip ------------------------------------------------------------------------------

rowsByGid <- lapply(index, \(x) split(x, x$gid)) |> unname() |> do.call(what = c)
expected <- lapply(seq_len(nrow(zips)), function(i) {
  .archiveExpected(rowsByGid[[zips$gid[i]]], zips$type[i])
})

## mclapply() returns a worker's error as a "try-error" value instead of stopping
stopOnWorkerErrors <- function(x, what) {
  bad <- vapply(x, inherits, logical(1), "try-error")
  if (any(bad)) {
    stop(what, " failed for ", sum(bad), " zips; first error: ", x[[which(bad)[1]]], call. = FALSE)
  }
  x
}

## local zips: MD5 and listing, in parallel (reads from the share)
## a zip that can't be read is reported (problem "unreadable_zip") and checked as empty,
## so its folders show as missing, rather than stopping the audit
readListing <- function(f) {
  l <- tryCatch(.zipListing(f), error = \(e) e)
  if (inherits(l, "error")) {
    return(list(
      listing = data.frame(path = character(0), size = numeric(0)),
      error = conditionMessage(l)
    ))
  }
  list(listing = l, error = NA_character_)
}

message("Reading local zips")
local <- parallel::mclapply(
  file.path(auditZipRoot, zips$zipfile),
  function(f) {
    if (!file.exists(f)) {
      return(list(md5 = NA_character_, listing = NULL, error = NA_character_))
    }
    c(list(md5 = unname(tools::md5sum(f))), readListing(f))
  },
  mc.cores = nCores
) |>
  stopOnWorkerErrors("Reading the local zip")
zips$md5_local <- vapply(local, `[[`, character(1), "md5")

## zips whose local copy is missing or differs from Drive: check the Drive copy
useLocal <- !is.na(zips$md5_local) & !is.na(zips$md5_drive) & zips$md5_local == zips$md5_drive
toDownload <- which(!useLocal & zips$on_drive)
message("Downloading ", length(toDownload), " zips whose local copy is missing or differs")
listings <- lapply(local, `[[`, "listing")
unreadable <- vapply(local, `[[`, character(1), "error")
unreadable[!useLocal] <- NA_character_
zips$source <- ifelse(useLocal, "local", "none")
for (i in toDownload) {
  dl <- file.path(downloadDir, basename(zips$zipfile[i]))
  ok <- tryCatch(
    {
      googledrive::drive_download(googledrive::as_id(zips$gid[i]), path = dl, overwrite = TRUE)
      TRUE
    },
    error = function(e) {
      message("Download failed for ", zips$zipfile[i], ": ", conditionMessage(e))
      FALSE
    }
  )
  if (ok) {
    r <- readListing(dl)
    listings[[i]] <- r$listing
    unreadable[i] <- r$error
    zips$source[i] <- "drive"
  }
}

## Expected names are those ClimateNA v7.60+ writes. Folders still holding the pre-v7.60
## radiation names (Rad*, MAR) are checked against those names, so missing data stays
## visible; they are counted in `legacy_folders`, and their zips need regenerating.
checkListing <- function(listing, expected) {
  e <- .archiveExpectedByVersion(listing, expected)
  list(problems = .archiveCheck(listing, e$expected), legacy = length(e$legacyFolders))
}

checks <- lapply(seq_len(nrow(zips)), function(i) {
  if (zips$source[i] == "none") {
    return(list(problems = data.frame(problem = character(0), item = character(0)), legacy = NA))
  }
  checkListing(listings[[i]], expected[[i]])
})
problems <- lapply(checks, `[[`, "problems")
zips$n_expected <- lengths(expected)
zips$n_files <- vapply(listings, \(l) if (is.null(l)) NA_integer_ else nrow(l), integer(1))
zips$legacy_folders <- vapply(checks, \(x) as.integer(x$legacy), integer(1))
zips$complete <- ifelse(zips$source == "none", NA, vapply(problems, .archiveComplete, logical(1)))
zips$local_complete <- vapply(
  seq_len(nrow(zips)),
  function(i) {
    l <- local[[i]]$listing
    if (is.null(l)) NA else .archiveComplete(checkListing(l, expected[[i]])$problems)
  },
  logical(1)
)

## keep downloads only for incomplete zips: they are the backup source for the repair
for (i in which(zips$source == "drive" & zips$complete %in% TRUE)) {
  unlink(file.path(downloadDir, basename(zips$zipfile[i])))
}

# raw outputs for incomplete zips ---------------------------------------------------------------

## the tile directory, without ClimateNA_path(), which creates directories
tileDirOf <- function(i) file.path(ClimateNAdata, dirname(zips$zipfile[i]), zips$tileid[i])

folderTime <- function(folder, msy) {
  sub(paste0(msy, "$"), "", sub("^.*(@|Year_|Normal_)", "", folder))
}

incomplete <- which(zips$complete %in% FALSE)
## a release's raw outputs are the pipeline's, in its own layout: it reruns anything missing
raw <- if (releaseAudit) {
  list()
} else {
  message("Checking raw outputs for ", length(incomplete), " incomplete zips")
  parallel::mclapply(
    incomplete,
    function(i) {
      folders <- unique(dirname(expected[[i]]))
      checkListing(.dirListing(tileDirOf(i), folders), expected[[i]])$problems
    },
    mc.cores = nCores
  ) |>
    stopOnWorkerErrors("Checking the raw outputs")
}
## complete zips made before ClimateNA v7.60 need regenerating for the rsds names
## zips not checked (missing on Drive, or the download failed) get no repair class
zips$repair <- ifelse(
  !zips$complete %in% TRUE,
  NA_character_,
  ifelse(zips$legacy_folders %in% 0L, "none", "regenerate_v760")
)
zips$repair[incomplete] <- if (releaseAudit) {
  rep("rerun_pipeline", length(incomplete))
} else {
  ifelse(vapply(raw, .archiveComplete, logical(1)), "repairable", "needs_climatena")
}

reruns <- lapply(seq_along(raw), function(k) {
  i <- incomplete[k]
  p <- raw[[k]]
  p <- p[p$problem %in% .archiveFailing, , drop = FALSE]
  if (nrow(p) == 0) {
    return(NULL)
  }
  folder <- ifelse(p$problem == "missing_folder", p$item, dirname(p$item)) |> unique()
  data.frame(
    type = zips$type[i],
    tileid = zips$tileid[i],
    gcm = zips$gcm[i] %||% NA_character_,
    ssp = zips$ssp[i] %||% NA_character_,
    msy = zips$msy[i],
    year_or_period = folderTime(folder, zips$msy[i]),
    folder = folder
  )
}) |>
  dplyr::bind_rows()

# outputs -------------------------------------------------------------------------------------

decadeOf <- function(zipfile) {
  ifelse(grepl("_\\d{4}[.]zip$", zipfile), sub("^.*_(\\d{4})[.]zip$", "\\1", zipfile), NA)
}
zipCols <- function(i) {
  data.frame(
    gid = zips$gid[i],
    type = zips$type[i],
    zipfile = zips$zipfile[i],
    tileid = zips$tileid[i],
    gcm = zips$gcm[i] %||% NA_character_,
    ssp = zips$ssp[i] %||% NA_character_,
    msy = zips$msy[i],
    decade_or_period = decadeOf(zips$zipfile[i])
  )
}

report <- list(indexGaps)
for (k in seq_len(nrow(driveProblems))) {
  i <- match(driveProblems$gid[k], zips$gid)
  report[[length(report) + 1]] <- cbind(
    zipCols(i),
    problem = driveProblems$problem[k],
    item = basename(zips$zipfile[i])
  )
}
differs <- which(!is.na(zips$md5_local) & zips$on_drive & !useLocal)
for (i in differs) {
  report[[length(report) + 1]] <- cbind(
    zipCols(i),
    problem = "md5_differs",
    item = paste0("local ", zips$md5_local[i], " / drive ", zips$md5_drive[i])
  )
}
for (i in which(!is.na(unreadable))) {
  report[[length(report) + 1]] <- cbind(
    zipCols(i),
    problem = "unreadable_zip",
    item = unreadable[i]
  )
}
for (i in which(vapply(problems, nrow, integer(1)) > 0)) {
  report[[length(report) + 1]] <- cbind(zipCols(i), problems[[i]])
}
report <- dplyr::bind_rows(report)
report$repair <- zips$repair[match(report$gid, zips$gid)]

zips$checked <- checked
zipsOut <- zips[c(
  "gid",
  "type",
  "zipfile",
  "tileid",
  intersect(c("gcm", "ssp"), names(zips)),
  "msy",
  "n_expected",
  "n_files",
  "size",
  "md5_drive",
  "md5_local",
  "source",
  "complete",
  "legacy_folders",
  "repair",
  "local_complete",
  "checked"
)]
utils::write.csv(zipsOut, file.path(auditDir, "zips.csv"), row.names = FALSE)
utils::write.csv(report, file.path(auditDir, "report.csv"), row.names = FALSE)
utils::write.csv(reruns, file.path(auditDir, "climatena_reruns.csv"), row.names = FALSE)

counts <- if (nrow(report) > 0) table(report$type, report$problem) else table(character(0))
summaryLines <- c(
  paste("# ClimateNA archive audit,", format(Sys.Date())),
  "",
  paste0("- Zips audited: ", nrow(zips), " (types: ", paste(auditTypes, collapse = ", "), ")"),
  paste0("- Tiles: ", if (is.null(auditTiles)) "all" else paste(auditTiles, collapse = ", ")),
  paste0("- Checked from the local copy: ", sum(zips$source == "local")),
  paste0("- Checked from a Drive download: ", sum(zips$source == "drive")),
  paste0("- Not checked (missing on Drive or download failed): ", sum(zips$source == "none")),
  paste0("- Complete: ", sum(zips$complete %in% TRUE)),
  paste0("  - up to date (ClimateNA v7.60+ names): ", sum(zips$repair %in% "none")),
  paste0(
    "  - to regenerate with ClimateNA v7.60+ (radiation named Rad*/MAR): ",
    sum(zips$repair %in% "regenerate_v760")
  ),
  paste0("- Incomplete: ", sum(zips$complete %in% FALSE)),
  paste0("  - repairable from raw outputs: ", sum(zips$repair %in% "repairable")),
  paste0("  - needing ClimateNA reruns: ", sum(zips$repair %in% "needs_climatena")),
  paste0("  - needing a pipeline rerun (a release): ", sum(zips$repair %in% "rerun_pipeline")),
  paste0("- Raw folders to rerun in ClimateNA: ", nrow(reruns)),
  paste0("- Total size on Drive: ", round(sum(zips$size, na.rm = TRUE) / 1024^3, 1), " GiB"),
  "",
  "## Problems by type",
  "",
  "```",
  utils::capture.output(print(counts)),
  "```",
  "",
  "## Repairable zips",
  "",
  if (any(zips$repair %in% "repairable")) {
    paste0("- ", zips$zipfile[zips$repair %in% "repairable"])
  } else {
    "None."
  }
)
writeLines(summaryLines, file.path(auditDir, "summary.md"))
message("Audit written to ", auditDir)
