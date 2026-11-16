## ClimateNA v7.60 (April 2025) replaced its RAD variables with rsds
## ("rsds based on Chelsa (relation based) to replace current RAD").
test_that("radiation variables use the ClimateNA v7.60 name rsds", {
  legacy <- c("MAR", sprintf("Rad%02d", 1:12), paste0("Rad_", c("at", "sm", "sp", "wt")))
  expect_contains(.allowedClimateVars_M, sprintf("rsds%02d", 1:12))
  expect_contains(.allowedClimateVars_S, paste0("rsds_", c("at", "sm", "sp", "wt")))
  expect_contains(.allowedClimateVars_Y, "rsds")
  expect_contains(.allowedClimateVars_nrm, "rsds")
  expect_length(intersect(.allowedClimateVars, legacy), 0)
  expect_length(.allowedClimateVars_MSY, 265)
})

test_that("historical years run to 2025 (ClimateNA v7.70)", {
  expect_identical(available("historical")$years, 1901L:2025L)
})
