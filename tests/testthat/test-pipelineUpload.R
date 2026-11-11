## What upload_zips() does with a zip, given the same-named files already in its release folder:
## reuse one with the same MD5; else replace a differing one in place, so its Drive ID (the
## index's gid) stays the same; else upload a new file.
noFiles <- data.frame(id = character(0), md5 = character(0))

test_that(".uploadAction reuses a same-named file with the same MD5", {
  there <- data.frame(id = c("a", "b"), md5 = c("x", "y"))
  expect_identical(.uploadAction(there, "y"), list(action = "found", id = "b"))
})

test_that(".uploadAction uploads a new file when none has the name", {
  expect_identical(.uploadAction(noFiles, "y"), list(action = "upload", id = NA_character_))
})

test_that(".uploadAction updates the one same-named file in place", {
  there <- data.frame(id = "a", md5 = "x")
  expect_identical(.uploadAction(there, "y"), list(action = "update", id = "a"))
  expect_identical(
    .uploadAction(data.frame(id = "a", md5 = NA_character_), "y"),
    list(action = "update", id = "a")
  )
})

test_that(".uploadAction updates the file the index points to when there are several", {
  there <- data.frame(id = c("a", "b"), md5 = c("x", "z"))
  expect_identical(.uploadAction(there, "y", gid = "b"), list(action = "update", id = "b"))
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
