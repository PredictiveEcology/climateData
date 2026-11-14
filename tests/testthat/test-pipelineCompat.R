ascLines <- function(v) {
  c(
    "ncols 3",
    "nrows 1",
    "xllcorner 0",
    "yllcorner 0",
    "cellsize 1",
    "NODATA_value -9999",
    paste(v, collapse = " ")
  )
}

## A new ClimateNA release that only differs by last-digit rounding (the Wine spike: 3 of 3.9
## million cells, each off by 1) reuses the existing outputs; a real change regenerates them.
test_that(".compatVerdict accepts last-digit rounding and rejects real changes", {
  skip_if_not_installed("withr")
  a <- withr::local_tempdir()
  b <- withr::local_tempdir()
  writeLines(ascLines(c(374, 10.5, 3)), file.path(a, "DD_0_at.asc"))
  writeLines(ascLines(c(375, 10.5, 3)), file.path(b, "DD_0_at.asc"))
  big <- seq_len(300000)
  writeLines(ascLines(big), file.path(a, "MAT.asc"))
  writeLines(ascLines(big), file.path(b, "MAT.asc"))
  cmp <- .compareFolders(a, b)
  expect_identical(cmp$differing[cmp$file == "DD_0_at.asc"], 1L)
  expect_identical(.compatVerdict(cmp), "compatible")

  writeLines(ascLines(c(374, 12, 3)), file.path(b, "DD_0_at.asc")) # off by more than 1
  expect_identical(.compatVerdict(.compareFolders(a, b)), "changed")
  file.remove(file.path(b, "MAT.asc")) # file set differs
  expect_identical(.compatVerdict(.compareFolders(a, b)), "changed")
})

test_that(".compareAsc ignores Windows line endings but not header changes", {
  skip_if_not_installed("withr")
  d <- withr::local_tempdir()
  writeLines(ascLines(c(1, 2, 3)), file.path(d, "lf.asc"))
  writeBin(
    charToRaw(paste0(paste(ascLines(c(1, 2, 3)), collapse = "\r\n"), "\r\n")),
    file.path(d, "crlf.asc")
  )
  expect_identical(.compareAsc(file.path(d, "lf.asc"), file.path(d, "crlf.asc"))$differing, 0L)
  writeLines(sub("cellsize 1", "cellsize 2", ascLines(c(1, 2, 3))), file.path(d, "other.asc"))
  expect_identical(.compareAsc(file.path(d, "lf.asc"), file.path(d, "other.asc"))$differing, 3L)
})
