test_that("archives table has the declared column types", {
  skip_if_not_installed("withr")
  db <- sqlite_connect_db(file.path(withr::local_tempdir(), "t.sqlite"))
  withr::defer(DBI::dbDisconnect(db))
  .archivesCreate(db)
  info <- DBI::dbGetQuery(db, "PRAGMA table_info(archives)")
  expect_equal(info$name, c("gid", "type", "zipfile", "md5", "n_files", "size", "verified"))
  expect_equal(info$type, c("TEXT", "TEXT", "TEXT", "TEXT", "INTEGER", "REAL", "REAL"))
})

test_that(".archivesUpsert replaces rows by gid", {
  skip_if_not_installed("withr")
  db <- sqlite_connect_db(file.path(withr::local_tempdir(), "t.sqlite"))
  withr::defer(DBI::dbDisconnect(db))
  .archivesCreate(db)
  df <- data.frame(
    gid = c("a", "b"),
    type = "future",
    zipfile = c("x/a.zip", "x/b.zip"),
    md5 = c("m1", "m2"),
    n_files = 265L,
    size = 3e9,
    verified = c(1.7e9, NA)
  )
  expect_equal(.archivesUpsert(db, df), 2L)
  df$md5[1] <- "m9"
  .archivesUpsert(db, df[1, ])
  out <- DBI::dbGetQuery(db, "SELECT gid, md5, size, verified FROM archives ORDER BY gid")
  expect_equal(out$md5, c("m9", "m2"))
  expect_equal(out$size, c(3e9, 3e9))
  expect_equal(is.na(out$verified), c(FALSE, TRUE))
})

test_that("shipped index: one archives row per published zip", {
  skip_if_not_installed("withr")
  f <- withr::local_tempfile(fileext = ".sqlite")
  file.copy(ClimateNA_tiles_sqlite(), f)
  con <- DBI::dbConnect(RSQLite::SQLite(), f, flags = RSQLite::SQLITE_RO)
  withr::defer(DBI::dbDisconnect(con))
  skip_if_not("archives" %in% DBI::dbListTables(con), "archives table not yet filled")
  gids <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT gid FROM historical UNION SELECT gid FROM historical_normals",
      "UNION SELECT gid FROM future"
    )
  )$gid
  a <- DBI::dbGetQuery(con, "SELECT gid, md5 FROM archives")
  expect_setequal(a$gid, gids)
  expect_match(a$md5, "^[0-9a-f]{32}$")
})

test_that("index tables declare a climatena_version column", {
  skip_if_not_installed("withr")
  f <- file.path(withr::local_tempdir(), "t.sqlite")
  for (type in c("historical", "historical_normals", "future", "future_normals")) {
    x <- ClimateNA_sql(f, type)
    expect_contains(DBI::dbListFields(x$db, type), "climatena_version")
    DBI::dbDisconnect(x$db)
  }
})
