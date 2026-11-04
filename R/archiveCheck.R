## Checks of the published ClimateNA zips against what the tile index says they hold.
## Used by the maintainer scripts data-raw/04-audit_archives.R and 05-repair_archives.R.
##
## A zip is complete when every expected folder is present and every expected variable
## file in it is present and non-empty. Small files and extra files are reported but
## don't make a zip incomplete.

.archiveFailing <- c("missing_folder", "missing_file", "empty_file")

## Member paths ("<folder>/<VAR>.asc") that the zip holding these index rows should contain.
.archiveExpected <- function(rows, type) {
  stopifnot(type %in% c("historical", "historical_normals", "future", "future_normals"))

  keys <- intersect(c("zipfile", "tileid", "msy", "gcm", "ssp"), names(rows))
  multi <- keys[vapply(rows[keys], \(x) length(unique(x)) > 1, logical(1))]
  if (length(multi) > 0) {
    stop(
      "Index rows for one archive",
      if (!is.null(rows$gid)) paste0(" (Drive ID ", paste(unique(rows$gid), collapse = ", "), ")"),
      " disagree on ",
      paste(multi, collapse = ", "),
      ": ",
      paste(unique(rows$zipfile), collapse = ", "),
      call. = FALSE
    )
  }

  msy <- rows$msy[1]
  period <- if (type %in% c("historical", "future")) rows$year else rows$period
  folders <- .periodFolder(type, period, msy, rows$gcm[1], rows$ssp[1]) |> unique() |> sort()

  vars <- if (type %in% c("historical_normals", "future_normals")) {
    .allowedClimateVars_nrm
  } else {
    switch(
      msy,
      M = .allowedClimateVars_M,
      S = .allowedClimateVars_S,
      Y = .allowedClimateVars_Y,
      MSY = .allowedClimateVars_MSY,
      stop("Unknown msy value: ", msy, call. = FALSE)
    )
  }

  outer(folders, paste0(vars, ".asc"), paste, sep = "/") |> t() |> as.vector()
}

## Radiation names used before ClimateNA v7.60 (April 2025), keyed by the current rsds names.
.legacyRadiationNames <- c(
  stats::setNames(sprintf("Rad%02d", 1:12), sprintf("rsds%02d", 1:12)),
  stats::setNames(
    paste0("Rad_", c("at", "sm", "sp", "wt")),
    paste0("rsds_", c("at", "sm", "sp", "wt"))
  ),
  rsds = "MAR"
)

## Expected member paths, using the pre-v7.60 radiation names in folders of `listing` that
## hold any of them. Such folders are complete data but need regenerating with v7.60, so
## they are returned separately (`legacyFolders`) instead of being reported as missing files.
.archiveExpectedByVersion <- function(listing, expected) {
  vars <- sub("[.]asc$", "", basename(listing$path))
  legacyFolders <- unique(dirname(listing$path[vars %in% .legacyRadiationNames]))

  ev <- sub("[.]asc$", "", basename(expected))
  swap <- dirname(expected) %in% legacyFolders & ev %in% names(.legacyRadiationNames)
  expected[swap] <- file.path(
    dirname(expected[swap]),
    paste0(.legacyRadiationNames[ev[swap]], ".asc")
  )
  list(expected = expected, legacyFolders = legacyFolders)
}

## File listing of a zip, without directory entries.
.zipListing <- function(zipfile) {
  ## utils::unzip() cannot open a zip with no entries (only the 22-byte end-of-archive record)
  if (.isEmptyZip(zipfile)) {
    return(data.frame(path = character(0), size = numeric(0)))
  }
  x <- utils::unzip(zipfile, list = TRUE)
  x <- x[!endsWith(x$Name, "/"), , drop = FALSE]
  data.frame(path = x$Name, size = as.numeric(x$Length))
}

.isEmptyZip <- function(zipfile) {
  identical(file.size(zipfile), 22) &&
    identical(readBin(zipfile, "raw", 4L), as.raw(c(0x50, 0x4b, 0x05, 0x06)))
}

## File listing of `folders` under `root`, with paths relative to `root`.
## Folders that don't exist contribute nothing.
.dirListing <- function(root, folders) {
  paths <- lapply(folders, function(f) {
    d <- file.path(root, f)
    if (dir.exists(d)) file.path(f, list.files(d, all.files = TRUE, no.. = TRUE)) else character(0)
  }) |>
    unlist()
  full <- file.path(root, paths)
  isFile <- !dir.exists(full)
  data.frame(path = paths[isFile], size = as.numeric(file.size(full[isFile])))
}

## Problems found when comparing a listing (path, size) with the expected member paths.
.archiveCheck <- function(listing, expected) {
  folders <- unique(dirname(expected))
  present <- vapply(folders, \(f) any(startsWith(listing$path, paste0(f, "/"))), logical(1))
  missingFolders <- folders[!present]

  inPresent <- dirname(expected) %in% folders[present]
  missingFiles <- setdiff(expected[inPresent], listing$path)

  known <- listing[listing$path %in% expected, , drop = FALSE]
  emptyFiles <- known$path[known$size == 0]

  ## a likely truncated write: under half the median size of the same variable in other folders
  var <- basename(known$path)
  med <- stats::ave(known$size, var, FUN = stats::median)
  n <- stats::ave(known$size, var, FUN = length)
  smallFiles <- known$path[n >= 2 & known$size > 0 & known$size < 0.5 * med]

  extra <- setdiff(listing$path, expected)

  problem <- function(type, items) data.frame(problem = rep(type, length(items)), item = items)
  rbind(
    problem("missing_folder", missingFolders),
    problem("missing_file", missingFiles),
    problem("empty_file", emptyFiles),
    problem("small_file", smallFiles),
    problem("extra", extra)
  )
}

.archiveComplete <- function(problems) {
  !any(problems$problem %in% .archiveFailing)
}

## Expected index rows (tile x msy [x gcm x ssp] x year or period) that the index lacks.
.archiveIndexGaps <- function(index, type) {
  av <- available(type)
  when <- if (is.null(av$years)) "period" else "year"
  grid <- list(tileid = unique(index$tileid), msy = unique(index$msy))
  if (type == "future") {
    grid <- c(grid, list(gcm = av$gcms, ssp = av$ssps))
  }
  grid[[when]] <- if (when == "year") av$years else av$periods
  full <- expand.grid(grid, stringsAsFactors = FALSE)

  keyCols <- names(grid)
  have <- do.call(paste, c(unname(as.list(index[keyCols])), sep = "\r"))
  want <- do.call(paste, c(unname(as.list(full[keyCols])), sep = "\r"))
  full <- full[!want %in% have, , drop = FALSE]
  rownames(full) <- NULL
  full
}

## Drive-side problems: IDs that are absent or trashed, and files whose name differs
## from the index's zip file name.
.driveProblems <- function(zips, meta) {
  m <- meta[match(zips$gid, meta$id), , drop = FALSE]
  problem <- ifelse(
    is.na(m$id) | m$trashed %in% TRUE,
    "drive_missing",
    ifelse(m$name != basename(zips$zipfile), "name_mismatch", NA_character_)
  )
  keep <- !is.na(problem)
  data.frame(gid = zips$gid[keep], problem = problem[keep])
}
