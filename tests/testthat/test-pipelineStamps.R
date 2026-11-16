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

## An error here fails the batch on every run, so a damaged period would never be regenerated.
test_that(".periodDone sends a damaged zip or a cut-short stamp back to be rerun", {
  skip_if_not_installed("withr")
  tileDir <- withr::local_tempdir()
  folder <- "Year_2025Y"
  expected <- file.path(folder, paste0(.allowedClimateVars_Y, ".asc"))
  .writeStamp(tileDir, folder, "7.70", "abc", "img")
  writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 1:40)), .periodZip(tileDir, folder)) # not a zip
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE)

  file.create(.stampPath(tileDir, folder)) # empty stamp
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE)
  writeLines("climatena_vers", .stampPath(tileDir, folder)) # cut mid-line
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE)
  writeLines("climatena_version: 7.", .stampPath(tileDir, folder)) # not a version
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE)
})
