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

## the pipeline side (data-raw/pipeline/R/fill.R), with ClimateNA replaced by a stub
fillEnv <- function() {
  d <- test_path("..", "..", "data-raw", "pipeline", "R")
  skip_if_not(dir.exists(d), "data-raw/pipeline not available")
  skip_if(Sys.which("zip") == "", "no zip program")
  e <- new.env()
  for (f in c("specs.R", "run_batch.R", "fill.R")) {
    sys.source(file.path(d, f), envir = e)
  }
  e$pipeline_load <- function() invisible(TRUE)
  e
}

## a 2 x 2 tile at x 0-2 (or 2-4), y 0-2; values row by row from the top left
writeGrid <- function(file, values, xmin = 0) {
  r <- terra::rast(nrows = 2, ncols = 2, xmin = xmin, xmax = xmin + 2, ymin = 0, ymax = 2)
  terra::crs(r) <- "EPSG:4326"
  terra::values(r) <- values
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(r, file, NAflag = -9999, overwrite = TRUE)
  unlink(paste0(file, ".aux.xml"))
  file
}

## a stored raw period (<tileDir>/<folder>.zip holding <folder>/MAT.asc) and its stamp
writePeriod <- function(tileDir, folder, mat, xmin = 0) {
  stage <- withr::local_tempdir(.local_envir = parent.frame())
  writeGrid(file.path(stage, folder, "MAT.asc"), mat, xmin)
  dir.create(tileDir, recursive = TRUE, showWarnings = FALSE)
  withr::with_dir(
    stage,
    utils::zip(
      file.path(tileDir, paste0(folder, ".zip")),
      file.path(folder, "MAT.asc"),
      flags = "-q"
    )
  )
  .writeStamp(tileDir, folder, "7.70", "dem", "image")
}

## stands in for ClimateNA's point mode: MAT = -1.5 at every point, and records each call
stubRunner <- function(calls) {
  function(points, msy, periodArg) {
    calls$n <- c(calls$n, periodArg)
    d <- tempfile("stub-")
    dir.create(d)
    out <- file.path(d, "out.csv")
    writeLines(
      c(
        "id1,id2,Latitude,Longitude,Elevation,MAT",
        paste(
          points$tile,
          points$cell,
          points$src_lat,
          points$src_lon,
          points$elev,
          "-1.5",
          sep = ","
        )
      ),
      out
    )
    out
  }
}

test_that("pipeline_fill_reference picks the last historical year and the first scenario year", {
  e <- fillEnv()
  cfg <- list(
    historical_years = list(1901, 2025),
    future_years = list(2011, 2100),
    gcms = list("CanESM5", "CNRM-ESM2-1"),
    ssps = list("245", "370")
  )
  cfg$types <- list("historical", "future", "historical_normals", "future_normals")
  cfg$historical_periods <- list("1901_1930", "1991_2020")
  cfg$future_periods <- list("2011_2040", "2071_2100")
  r <- e$pipeline_fill_reference(cfg)
  expect_identical(r$historical$period, 2025L)
  expect_identical(
    r$future[c("gcm", "ssp", "period")],
    list(gcm = "CanESM5", ssp = "245", period = 2011L)
  )
  expect_identical(c(r$historical$msy, r$future$msy), c("MSY", "MSY"))

  ## only the release's types: a class without yearly archives uses its normals; a class with
  ## neither has no reference (nothing to fill)
  cfg$types <- list("historical_normals")
  r <- e$pipeline_fill_reference(cfg)
  expect_named(r, "historical")
  expect_identical(
    r$historical[c("type", "period", "msy")],
    list(type = "historical_normals", period = "1991_2020", msy = "Y")
  )
})

test_that("pipeline_fill_points finds land cells empty in the reference and gives each a source", {
  e <- fillEnv()
  out <- withr::local_tempdir()
  demDir <- withr::local_tempdir()
  ## tile 1 is all land; tile 2's top-left cell has no DEM (ocean) but a ClimateNA value
  demFiles <- c(
    "1" = writeGrid(file.path(demDir, "can_dem_1.asc"), c(10, 20, 30, 40)),
    "2" = writeGrid(file.path(demDir, "can_dem_2.asc"), c(NA, 5, 6, 7), xmin = 2)
  )
  canada <- terra::vect("POLYGON ((-1 -1, 5 -1, 5 3, -1 3, -1 -1))", crs = "EPSG:4326")
  masks <- lapply(demFiles, \(f) .maskCells(terra::rast(f), canada))
  writePeriod(file.path(out, "raw", "historical", "1"), "Year_2025MSY", c(1, NA, 3, 4))
  writePeriod(file.path(out, "raw", "historical", "2"), "Year_2025MSY", c(9, 5, 6, 7), xmin = 2)
  ref <- list(
    historical = list(type = "historical", period = 2025L, msy = "MSY", gcm = NA, ssp = NA)
  )
  calls <- new.env()

  p <- e$pipeline_fill_points(
    "m.tsv",
    masks,
    demFiles,
    ref,
    out,
    maxKm = 200,
    run = stubRunner(calls)
  )
  expect_identical(nrow(p), 1L)
  expect_identical(p$class, "historical")
  expect_identical(c(p$tile, p$cell), c(1L, 2L))
  expect_identical(p$elev, "20")
  ## nearest land cell with values: tile 1's cell 4 (1.5, 0.5), not tile 2's ocean cell (2.5, 1.5)
  expect_equal(c(p$lon, p$lat, p$src_lon, p$src_lat), c(1.5, 1.5, 1.5, 0.5))
  expect_identical(calls$n, "Year_2025.ann")
})

test_that("fill_batch runs each period once, reuses its CSV, and skips tiles without points", {
  e <- fillEnv()
  out <- withr::local_tempdir()
  tileDir <- file.path(out, "raw", "historical", "1")
  for (y in 2020:2021) {
    writePeriod(tileDir, paste0("Year_", y, "MSY"), c(1, NA, 3, 4))
  }
  spec <- list(
    id = "h1",
    type = "historical",
    tile = 1L,
    gcm = NA,
    ssp = NA,
    msy = "MSY",
    decade = 2020L,
    periods = 2020:2021
  )
  pts <- data.frame(
    class = "historical",
    tile = 1L,
    cell = 2L,
    lon = 1.5,
    lat = 1.5,
    elev = "20",
    src_lon = 1.5,
    src_lat = 0.5,
    km = 110
  )
  calls <- new.env()
  run <- stubRunner(calls)

  f <- e$fill_batch(spec, "m.tsv", pts, out, run)
  expect_length(f, 2)
  expect_true(all(file.exists(f)))
  expect_match(basename(f), "^Year_202[01]MSY[.]fill-[0-9a-f]{8}[.]csv$")
  expect_identical(calls$n, c("Year_2020.ann", "Year_2021.ann"))

  expect_identical(e$fill_batch(spec, "m.tsv", pts, out, run), f)
  expect_length(calls$n, 2) # reused

  ## new sources: new files, and the old ones are removed
  pts$src_lon <- 0.5
  f2 <- e$fill_batch(spec, "m.tsv", pts, out, run)
  expect_false(any(f2 %in% f))
  expect_false(any(file.exists(f)))

  spec2 <- modifyList(spec, list(id = "h2", tile = 2L))
  expect_identical(e$fill_batch(spec2, "m.tsv", pts, out, run), character(0))
  expect_identical(e$fill_batch(spec, character(0), pts, out, run), character(0)) # failed batch
})
