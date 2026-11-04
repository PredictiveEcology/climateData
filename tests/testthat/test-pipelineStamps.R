## A period counts as done only when its folder is complete and its stamp shows a ClimateNA
## version at or above the type's minimum and the same DEM. A folder without a stamp (e.g. the
## pre-pipeline raw tree) is rerun, not skipped.
test_that(".periodDone needs a complete folder and a matching stamp", {
  skip_if_not_installed("withr")
  tileDir <- withr::local_tempdir()
  folder <- "Year_2025Y"
  expected <- file.path(folder, paste0(.allowedClimateVars_Y, ".asc"))
  dir.create(file.path(tileDir, folder))
  for (f in expected) {
    writeLines("1", file.path(tileDir, f))
  }

  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE) # no stamp
  .writeStamp(tileDir, folder, "7.70", "abc", "img@sha256:1")
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), TRUE)
  expect_identical(.periodDone(tileDir, folder, expected, "7.60", "abc"), TRUE) # made after the minimum
  expect_identical(.periodDone(tileDir, folder, expected, "7.80", "abc"), FALSE) # older than the minimum
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "xyz"), FALSE) # other DEM
  unlink(file.path(tileDir, expected[1]))
  expect_identical(.periodDone(tileDir, folder, expected, "7.70", "abc"), FALSE) # incomplete
  expect_identical(.readStamp(tileDir, folder)$image, "img@sha256:1")
  expect_false(file.exists(file.path(tileDir, folder, paste0(folder, ".stamp")))) # sibling, not inside
})
