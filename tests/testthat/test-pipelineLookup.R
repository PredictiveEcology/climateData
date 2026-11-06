## Adding a GCM or SSP changes specs_all, and with it the previous release's rows the lookup
## holds. The lookup's value must not change with them, or every upload branch reruns (24,518
## for release cna-v7.70-3arcmin). The pipeline code is in data-raw/, which R CMD check omits.
test_that("the previous-release lookup's value doesn't change when scenarios are added", {
  f <- test_path("..", "..", "data-raw", "pipeline", "R", "upload_drive.R")
  skip_if_not(file.exists(f), "data-raw/pipeline not available")
  skip_if_not_installed("withr")
  e <- new.env()
  sys.source(f, envir = e)
  e$pipeline_previous_archives <- function(index) {
    data.frame(zipfile = c("a.zip", "b.zip"), gid = c("ga", "gb"), md5 = c("ma", "mb"))
  }
  e$pipeline_zip_relpaths <- function(spec) spec$zips
  out <- withr::local_tempdir()

  one <- e$pipeline_previous_lookup("idx", list(list(zips = "a.zip")), out)
  two <- e$pipeline_previous_lookup("idx", list(list(zips = "a.zip"), list(zips = "b.zip")), out)
  expect_identical(one, two)

  rows <- e$pipeline_previous_rows(two, list(zips = "b.zip"))
  expect_identical(rows$gid, "gb")
})
