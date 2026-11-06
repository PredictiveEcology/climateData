test_that(".climatenaDockerArgs mounts ClimateNA read-only and passes container paths", {
  a <- .climatenaDockerArgs(
    name = "cna-r1-historical_39_2020",
    image = "ghcr.io/predictiveecology/climatena-wine:latest",
    climatenaDir = "/share/ClimateNA_v7.70",
    demFile = "/share/dem/can_dem_39.asc",
    outDir = "/share/raw/historical/39",
    exe = "ClimateNA_v7.70.exe",
    msy = "MSY",
    periodArg = "Year_2025.ann",
    uid = 1000L,
    gid = 1000L
  )
  expect_identical(a[1:4], c("run", "--rm", "--name", "cna-r1-historical_39_2020"))
  expect_contains(a, c("--cpus", "1", "--memory", "1g", "--user", "1000:1000"))
  expect_contains(a, "/share/ClimateNA_v7.70:/opt/climatena:ro")
  expect_contains(a, c("/share/dem:/data/dem:ro", "/share/raw/historical/39:/data/out"))
  expect_identical(
    tail(a, 6),
    c(
      "ghcr.io/predictiveecology/climatena-wine:latest",
      "ClimateNA_v7.70.exe",
      "MSY",
      "Year_2025.ann",
      "/data/dem/can_dem_39.asc",
      "/data/out"
    )
  )
})
