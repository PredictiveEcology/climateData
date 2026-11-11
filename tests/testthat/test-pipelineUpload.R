## What upload_zips() does with a zip, given the same-named files already in its release folder:
## reuse one with the same MD5; else, when asked (replace_changed in _local.R), replace a
## differing one in place, so its Drive ID (the index's gid) stays the same; else upload a new
## file.
noFiles <- data.frame(id = character(0), md5 = character(0))

test_that(".uploadAction reuses a same-named file with the same MD5", {
  there <- data.frame(id = c("a", "b"), md5 = c("x", "y"))
  expect_identical(.uploadAction(there, "y"), list(action = "found", id = "b"))
})

test_that(".uploadAction uploads a new file when none has the name", {
  expect_identical(.uploadAction(noFiles, "y"), list(action = "upload", id = NA_character_))
})

## a shipped release's files change only when the maintainer asks: an accidental recut stops
test_that(".uploadAction stops on a differing same-named file unless asked to replace it", {
  expect_error(.uploadAction(data.frame(id = "a", md5 = "x"), "y"), "replace_changed")
  expect_error(
    .uploadAction(data.frame(id = c("a", "b"), md5 = c("x", "z")), "y", gid = "b"),
    "replace_changed"
  )
})

test_that(".uploadAction updates the one same-named file in place", {
  there <- data.frame(id = "a", md5 = "x")
  expect_identical(.uploadAction(there, "y", replace = TRUE), list(action = "update", id = "a"))
  expect_identical(
    .uploadAction(data.frame(id = "a", md5 = NA_character_), "y", replace = TRUE),
    list(action = "update", id = "a")
  )
})

test_that(".uploadAction updates the file the index points to when there are several", {
  there <- data.frame(id = c("a", "b"), md5 = c("x", "z"))
  expect_identical(
    .uploadAction(there, "y", gid = "b", replace = TRUE),
    list(action = "update", id = "b")
  )
})

test_that(".uploadAction stops when several same-named files differ and none is the index's", {
  there <- data.frame(id = c("a", "b"), md5 = c("x", "z"))
  expect_error(.uploadAction(there, "y"), "a, b")
  expect_error(.uploadAction(there, "y", gid = "c"), "a, b")
})

## upload_zips() runs on the "uploads" workers, which start without the package, and it calls
## .uploadAction(): it must load the package first, as zip_batch() does. Without it, every
## changed zip's upload failed with "could not find function '.uploadAction'".
test_that("upload_zips loads the package on its worker", {
  f <- test_path("..", "..", "data-raw", "pipeline", "R", "upload_drive.R")
  skip_if_not(file.exists(f), "data-raw/pipeline not available")
  e <- new.env()
  sys.source(f, envir = e)
  loaded <- FALSE
  e$pipeline_load <- function() loaded <<- TRUE
  out <- e$upload_zips(character(0), list(), list(), dryRun = FALSE, lookup = NULL)
  expect_true(loaded)
  expect_identical(nrow(out), 0L)
})

## a rebuilt zip whose release folder already has a differing file of that name: replaced in
## place (same ID) only with replace_changed; otherwise the upload stops, naming the zip
test_that("upload_zips replaces a changed zip's file only when asked", {
  f <- test_path("..", "..", "data-raw", "pipeline", "R", "upload_drive.R")
  skip_if_not(file.exists(f), "data-raw/pipeline not available")
  e <- new.env()
  sys.source(f, envir = e)
  rel <- "historical/MSY/6_MSY_2020.zip"
  e$pipeline_load <- function() invisible(TRUE)
  e$pipeline_zip_relpath <- function(spec, subset) rel
  e$pipeline_previous_rows <- function(lookup, spec) {
    data.frame(gid = "g1", zipfile = rel, md5 = "old")
  }
  d <- withr::local_tempdir()
  zip <- file.path(d, "MSY", "6_MSY_2020.zip")
  dir.create(dirname(zip))
  writeLines("new", zip)
  md5 <- unname(tools::md5sum(zip))
  folders <- list(path = file.path(d, "folders.rds"))
  saveRDS(c("historical/MSY" = "folder1"), folders$path)
  there <- data.frame(name = basename(zip), id = "g1")
  there$drive_resource <- list(list(md5Checksum = "old"))
  updated <- data.frame(name = basename(zip), id = "g1")
  updated$drive_resource <- list(list(md5Checksum = md5, modifiedTime = "2026-11-11T00:00:00Z"))
  local_mocked_bindings(
    drive_get = function(...) stop("no reusable file"),
    drive_ls = function(...) there,
    drive_update = function(...) updated,
    .package = "googledrive"
  )

  expect_error(e$upload_zips(zip, list(), folders, FALSE, NULL), "6_MSY_2020.zip.*replace_changed")
  out <- e$upload_zips(zip, list(), folders, FALSE, NULL, replaceChanged = TRUE)
  expect_identical(out$source, "updated")
  expect_identical(out$gid, "g1")
  expect_identical(out$md5, md5)
})
