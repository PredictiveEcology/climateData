## The pipeline uses at most half of each node's cores (maintainer, 2026-11-10): ClimateNA workers
## (one CPU each, `--cpus 1`), plus the upload workers and the main process on the controlling node.
## The last 1 arcmin run had 60 workers on a 96-core node, next to another project's.
test_that(".cappedWorkers clamps to half the cores, less those reserved", {
  expect_message(n <- .cappedWorkers(60, 96, 9), "39")
  expect_identical(n, 39L)
})

test_that(".cappedWorkers keeps a request within the cap, silently", {
  expect_silent(n <- .cappedWorkers(10, 96, 9))
  expect_identical(n, 10L)
})

test_that(".cappedWorkers keeps at least one worker", {
  expect_message(n <- .cappedWorkers(4, 4, 3))
  expect_identical(n, 1L)
})
