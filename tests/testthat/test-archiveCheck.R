rowsH <- function(years, msy = "Y", tile = 39L, zip = "historical/yearly/39_Y_2010.zip") {
  data.frame(zipfile = zip, tileid = tile, msy = msy, year = years)
}

test_that(".archiveExpected names historical, future and normals members", {
  h <- .archiveExpected(rowsH(2013:2014), "historical")
  expect_length(h, 50)
  expect_contains(h, "Year_2013Y/AHM.asc")

  f <- .archiveExpected(
    data.frame(
      zipfile = "z",
      tileid = 39L,
      msy = "MSY",
      year = 2013L,
      gcm = "CNRM-ESM2-1",
      ssp = "370"
    ),
    "future"
  )
  expect_length(f, 265)
  expect_contains(f, "CNRM-ESM2-1_ssp370@2013MSY/Tmin12.asc")

  n <- .archiveExpected(
    data.frame(zipfile = "z", tileid = 39L, msy = "Y", period = "1991_2020"),
    "historical_normals"
  )
  expect_length(n, 25)
  expect_contains(n, "Normal_1991_2020Y/MAT.asc")
})

test_that(".archiveExpected stops when one zip's index rows disagree", {
  rows <- rbind(rowsH(2013), rowsH(2014, zip = "historical/yearly/39_Y_2020.zip"))
  expect_snapshot(.archiveExpected(rows, "historical"), error = TRUE)
})

test_that(".archiveCheck reports whole missing folders once (tile 39 shape)", {
  expected <- .archiveExpected(rowsH(2011:2019), "historical")
  present <- grep("Year_(2011|2012|2017|2018|2019)Y/", expected, value = TRUE)
  p <- .archiveCheck(data.frame(path = present, size = 100), expected)
  expect_equal(p$problem, rep("missing_folder", 4))
  expect_equal(p$item, paste0("Year_", 2013:2016, "Y"))
  expect_false(.archiveComplete(p))
})

test_that(".archiveCheck reports missing and empty files in present folders", {
  expected <- .archiveExpected(rowsH(2011), "historical")
  listing <- data.frame(path = expected[-1], size = c(0, rep(100, length(expected) - 2)))
  p <- .archiveCheck(listing, expected)
  expect_setequal(p$problem, c("missing_file", "empty_file"))
  expect_equal(p$item[p$problem == "missing_file"], expected[1])
  expect_equal(p$item[p$problem == "empty_file"], expected[2])
})

test_that("junk files are extra, not failing", {
  expected <- .archiveExpected(rowsH(2011), "historical")
  listing <- data.frame(path = c(expected, "Year_2011Y/Thumbs.db"), size = 100)
  p <- .archiveCheck(listing, expected)
  expect_equal(p$problem, "extra")
  expect_equal(.archiveComplete(p), TRUE)
})

test_that("small files are flagged against the same variable in other folders", {
  expected <- .archiveExpected(rowsH(2011:2013), "historical")
  size <- ifelse(expected == "Year_2012Y/MAT.asc", 400, 1000)
  p <- .archiveCheck(data.frame(path = expected, size = size), expected)
  expect_equal(p$problem, "small_file")
  expect_equal(p$item, "Year_2012Y/MAT.asc")
  expect_equal(.archiveComplete(p), TRUE)

  one <- .archiveExpected(rowsH(2100), "historical")
  expect_equal(nrow(.archiveCheck(data.frame(path = one, size = 1), one)), 0L)
})

test_that(".zipListing and .dirListing return relative paths and sizes", {
  skip_if_not_installed("archive")
  skip_if_not_installed("withr")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "Year_2011Y"))
  writeLines("x", file.path(root, "Year_2011Y", "MAT.asc"))
  z <- file.path(withr::local_tempdir(), "t.zip")
  archive::archive_write_dir(z, root)

  expect_equal(.zipListing(z)$path, "Year_2011Y/MAT.asc")
  d <- .dirListing(root, c("Year_2011Y", "Year_2012Y"))
  expect_equal(d$path, "Year_2011Y/MAT.asc")
  expect_gt(d$size, 0)
})

## Archives made before ClimateNA v7.60 name radiation Rad*/MAR instead of rsds*. Their data
## are complete, but they need regenerating; that is reported apart from missing data.
test_that(".archiveExpectedByVersion uses pre-v7.60 radiation names where a folder has them", {
  expected <- .archiveExpected(rowsH(2022:2023), "historical")
  old2022 <- sub("rsds[.]asc$", "MAR.asc", grep("Year_2022Y/", expected, value = TRUE))
  listing <- data.frame(path = c(old2022, grep("Year_2023Y/", expected, value = TRUE)), size = 100)
  e <- .archiveExpectedByVersion(listing, expected)
  expect_equal(e$legacyFolders, "Year_2022Y")
  expect_equal(nrow(.archiveCheck(listing, e$expected)), 0L)

  expectedM <- .archiveExpected(rowsH(2022, msy = "M"), "historical")
  oldM <- sub("rsds(\\d{2})[.]asc$", "Rad\\1.asc", expectedM)
  listingM <- data.frame(path = setdiff(oldM, "Year_2022M/Rad12.asc"), size = 100)
  eM <- .archiveExpectedByVersion(listingM, expectedM)
  p <- .archiveCheck(listingM, eM$expected)
  expect_equal(p$problem, "missing_file")
  expect_equal(p$item, "Year_2022M/Rad12.asc")
})

## Published as 9_CanESM5_370_MSY_2100.zip: 22 bytes, an end-of-archive record and nothing
## else. utils::unzip() cannot open it, which stopped the first full audit.
test_that(".zipListing reads an empty zip as an empty listing", {
  skip_if_not_installed("withr")
  z <- withr::local_tempfile(fileext = ".zip")
  writeBin(as.raw(c(0x50, 0x4b, 0x05, 0x06, rep(0, 18))), z)

  l <- .zipListing(z)
  expect_equal(nrow(l), 0L)
  expected <- .archiveExpected(rowsH(2100), "historical")
  p <- .archiveCheck(l, expected)
  expect_equal(p$problem, "missing_folder")
  expect_equal(p$item, "Year_2100Y")
})

test_that(".archiveIndexGaps finds years the index lacks", {
  idx <- rowsH(setdiff(available("historical")$years, 2013L))
  g <- .archiveIndexGaps(idx, "historical")
  expect_equal(g$year, 2013L)
  expect_equal(g$tileid, 39L)
})

test_that(".driveProblems classifies missing, trashed and renamed files", {
  zips <- data.frame(
    gid = c("a", "b", "c", "d"),
    zipfile = paste0("x/", c("a", "b", "c", "d"), ".zip")
  )
  meta <- data.frame(
    id = c("a", "b", "c"),
    name = c("a.zip", "b.zip", "renamed.zip"),
    md5 = "m",
    size = 1,
    trashed = c(FALSE, TRUE, FALSE)
  )
  p <- .driveProblems(zips, meta)
  expect_equal(p$gid, c("b", "c", "d"))
  expect_equal(p$problem, c("drive_missing", "name_mismatch", "drive_missing"))
})
