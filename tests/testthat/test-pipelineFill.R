## Land cells ClimateNA leaves empty are filled with its point-mode output at the nearest land
## cell it covers, run with the empty cell's own elevation.

test_that(".fillCandidates orders sources by distance and drops those beyond maxKm", {
  t <- data.frame(id = "a", lon = 0, lat = 60)
  s <- data.frame(id = c("far", "near", "out"), lon = c(0.2, 0.05, 2), lat = 60)
  cand <- .fillCandidates(t, s, maxKm = 50)
  expect_identical(cand$id, c("a", "a"))
  expect_identical(cand$rank, 1:2)
  expect_equal(cand$src_lon, c(0.05, 0.2))
  expect_equal(cand$km[1], terra::distance(cbind(0, 60), cbind(0.05, 60), lonlat = TRUE)[1] / 1000)
})

test_that(".fillCandidates gives a target with no source in reach no rows", {
  t <- data.frame(id = c("a", "b"), lon = c(0, 10), lat = 60)
  s <- data.frame(id = "s", lon = 0.01, lat = 60)
  cand <- .fillCandidates(t, s, maxKm = 50)
  expect_identical(unique(cand$id), "a")
})

test_that(".chooseSources moves to the next candidate when a point run is empty", {
  cand <- data.frame(
    id = c("a", "a", "b"),
    rank = c(1L, 2L, 1L),
    src_lon = c(1, 2, 3),
    src_lat = 60,
    km = c(1, 2, 3)
  )
  tried <- list()
  check <- function(df) {
    tried[[length(tried) + 1]] <<- df$id
    df$id[df$src_lon != 1] # the first source of "a" gives no values
  }
  s <- .chooseSources(cand, check)
  expect_equal(s$src_lon[s$id == "a"], 2)
  expect_equal(s$km[s$id == "a"], 2)
  expect_equal(s$src_lon[s$id == "b"], 3)
  ## rank 1 for both, then rank 2 only for the one that failed
  expect_identical(tried, list(c("a", "b"), "a"))
})

## ClimateNA's point mode reads a CSV of points and writes one, under Wine in the same image;
## the image's climatena-run wrapper would make the output path a folder, so sh runs wine itself
test_that(".climatenaPointDockerArgs mounts the work dir and runs wine on the CSVs", {
  a <- .climatenaPointDockerArgs(
    name = "cna-fill-1",
    image = "ghcr.io/predictiveecology/climatena-wine:latest",
    climatenaDir = "/share/ClimateNA_v7.70",
    workDir = "/scratch/work/fill-1",
    exe = "ClimateNA_v7.70.exe",
    msy = "MSY",
    periodArg = "Year_2020.ann",
    inCsv = "in.csv",
    outCsv = "out.csv",
    uid = 1000L,
    gid = 1000L
  )
  expect_identical(a[1:4], c("run", "--rm", "--name", "cna-fill-1"))
  expect_contains(a, c("--cpus", "1", "--memory", "1g", "--user", "1000:1000"))
  expect_contains(a, c("/share/ClimateNA_v7.70:/opt/climatena:ro", "/scratch/work/fill-1:/data/w"))
  i <- match("--entrypoint", a)
  expect_identical(a[i + 1], "sh")
  expect_identical(a[length(a) - 2], "ghcr.io/predictiveecology/climatena-wine:latest")
  expect_identical(a[length(a) - 1], "-c")
  script <- a[length(a)]
  expect_match(
    script,
    'wine ClimateNA_v7.70.exe /MSY /Year_2020.ann "/Z:\\data\\w\\in.csv" "/Z:\\data\\w\\out.csv"',
    fixed = TRUE
  )
  expect_match(script, "cd /opt/climatena", fixed = TRUE)
  ## system2() passes arguments to the host shell as they are: the script must reach the
  ## container whole, with $WINEPREFIX unexpanded
  expect_identical(script, shQuote(script_text <- sub("^'(.*)'$", "\\1", script)))
  expect_match(script_text, '"$WINEPREFIX"', fixed = TRUE)
})

test_that(".writePointCsv writes CRLF lines and the elevation text as given", {
  f <- withr::local_tempfile(fileext = ".csv")
  .writePointCsv(
    f,
    data.frame(
      tile = c(53L, 53L),
      cell = c(534L, 1591L),
      src_lat = c(66.51, 66.47),
      src_lon = c(-61.52, -61.525),
      elev = c("32", "280.4286")
    )
  )
  expect_identical(
    rawToChar(readBin(f, "raw", file.size(f))),
    "id1,id2,lat,long,elev\r\n53,534,66.51,-61.52,32\r\n53,1591,66.47,-61.525,280.4286\r\n"
  )
})

pointCsv <- function(lines) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeBin(
    charToRaw(paste0(
      c("id1,id2,Latitude,Longitude,Elevation,MAT,MAP", lines),
      "\r\n",
      collapse = ""
    )),
    f
  )
  f
}

test_that(".pointValues matches rows by cell, not position", {
  f <- pointCsv(c("53,7,66.5,-61.5,32,-6.8,250", "63,3,66.4,-61.5,280,-7.9,260"))
  v <- .pointValues(f, c("MAP", "MAT"))
  expect_identical(v$tile, c(53L, 63L))
  expect_identical(v$cells, c(7L, 3L))
  expect_identical(
    v$values,
    matrix(c("250", "260", "-6.8", "-7.9"), 2, dimnames = list(NULL, c("MAP", "MAT")))
  )
})

test_that(".pointValues drops -9999 rows with a warning", {
  f <- pointCsv(c("53,7,66.5,-61.5,32,-6.8,250", "53,3,66.4,-61.5,280,-9999,-9999"))
  expect_warning(v <- .pointValues(f, c("MAT", "MAP")), "no values.*cell 3")
  expect_identical(v$cells, 7L)
  expect_identical(unname(v$values[1, ]), c("-6.8", "250"))
})

test_that(".chooseSources leaves a target without a working candidate unfilled", {
  cand <- data.frame(id = "a", rank = 1L, src_lon = 1, src_lat = 60, km = 1)
  s <- .chooseSources(cand, function(df) character(0))
  expect_identical(s$id, "a")
  expect_true(is.na(s$src_lon) && is.na(s$src_lat) && is.na(s$km))
})
