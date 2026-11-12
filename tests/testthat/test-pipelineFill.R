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

test_that(".chooseSources leaves a target without a working candidate unfilled", {
  cand <- data.frame(id = "a", rank = 1L, src_lon = 1, src_lat = 60, km = 1)
  s <- .chooseSources(cand, function(df) character(0))
  expect_identical(s$id, "a")
  expect_true(is.na(s$src_lon) && is.na(s$src_lat) && is.na(s$km))
})
