## latestHistoricalYear() and the tile lookup run in many processes at once (every parse of a
## fireSense module calls it). They must only read the index, so concurrent opens must not fail with
## "database is locked".

skip_on_os("windows") ## uses forked workers

dbCopy <- function() {
  f <- tempfile(fileext = ".sqlite")
  file.copy(ClimateNA_tiles_sqlite(), f)
  f
}

test_that("parallel latestHistoricalYear() calls on one database do not fail", {
  f <- dbCopy()
  on.exit(unlink(paste0(f, c("", "-wal", "-shm"))), add = TRUE)
  testthat::local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)
  expected <- latestHistoricalYear()

  ## R CMD check caps forked workers at 2 (_R_CHECK_LIMIT_CORES_)
  nWorkers <- if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) 2L else 8L
  res <- parallel::mclapply(seq_len(nWorkers), function(i) {
    t0 <- Sys.time()
    out <- character()
    while (difftime(Sys.time(), t0, units = "secs") < 10) {
      r <- tryCatch(identical(latestHistoricalYear(), expected), error = function(e) conditionMessage(e))
      if (!isTRUE(r)) out <- c(out, as.character(r))
    }
    out
  }, mc.cores = nWorkers)

  errs <- unlist(res)
  expect_identical(unique(errs), character(0), info = paste(head(errs, 3), collapse = "; "))
})

test_that("a read-only lookup neither writes nor creates tables", {
  f <- dbCopy()
  on.exit(unlink(paste0(f, c("", "-wal", "-shm"))), add = TRUE)

  dbdf <- ClimateNA_sql(f, "historical", readonly = TRUE)
  expect_error(DBI::dbExecute(dbdf[["db"]], "DELETE FROM historical"), "readonly")
  DBI::dbDisconnect(dbdf[["db"]])

  ## a table that does not exist is not created on a read path
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS future_normals")
  DBI::dbDisconnect(con)
  expect_error(ClimateNA_sql(f, "future_normals", readonly = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false(DBI::dbExistsTable(con, "future_normals"))
})

test_that("a lookup waits for a lock held by another process instead of failing", {
  f <- dbCopy()
  on.exit(unlink(paste0(f, c("", "-wal", "-shm"))), add = TRUE)
  ## rollback-journal mode, where a writer's lock blocks readers
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbExecute(con, "PRAGMA journal_mode = DELETE")
  DBI::dbDisconnect(con)
  testthat::local_mocked_bindings(ClimateNA_tiles_sqlite = function() f)

  holder <- parallel::mcparallel({
    con <- DBI::dbConnect(RSQLite::SQLite(), f)
    DBI::dbExecute(con, "BEGIN EXCLUSIVE")
    Sys.sleep(2)
    DBI::dbExecute(con, "COMMIT")
    DBI::dbDisconnect(con)
  })
  Sys.sleep(0.5) ## let the holder take its lock
  expect_no_error(latestHistoricalYear())
  parallel::mccollect(holder)
})
