## A period counts as done only when its stored zip is complete and its stamp shows a ClimateNA
## version at or above the type's minimum and the same DEM. A period without a stamp, or stored
## only as a folder (the earlier layout), is rerun, not skipped.
test_that(".periodDone needs a complete period zip and a matching stamp", {
  skip_if_not_installed("withr")
  skip_if_not(nzchar(Sys.which("zip")), "no zip program")
  tileDir <- withr::local_tempdir()
  folder <- "Year_2025Y"
  expected <- file.path(folder, paste0(.allowedClimateVars_Y, ".asc"))
  storePeriod <- function(files) {
    src <- withr::local_tempdir()
    dir.create(file.path(src, folder))
    for (f in files) {
      writeLines("1", file.path(src, f))
    }
    unlink(.periodZip(tileDir, folder))
    withr::with_dir(src, utils::zip(.periodZip(tileDir, folder), files, flags = "-q"))
  }

  storePeriod(expected)
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE) # no stamp
  .writeStamp(tileDir, folder, "7.70", "abc", "img@sha256:1")
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), TRUE)
  expect_identical(.periodDone(tileDir, folder, expected, "7.60", "abc"), TRUE) # made after the minimum
  expect_identical(.periodDone(tileDir, folder, expected, "7.80", "abc"), FALSE) # older than the minimum
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "xyz"), FALSE) # other DEM

  storePeriod(expected[-1])
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE) # incomplete

  unlink(.periodZip(tileDir, folder))
  dir.create(file.path(tileDir, folder))
  for (f in expected) {
    writeLines("1", file.path(tileDir, f))
  }
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE) # folder only

  expect_identical(.readStamp(tileDir, folder)$image, "img@sha256:1")
  expect_identical(basename(.periodZip(tileDir, folder)), "Year_2025Y.zip") # sibling of the stamp
})
