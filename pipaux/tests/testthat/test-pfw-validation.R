library(testthat)

test_that("pfw validation works correctly", {
  expect_error(load_aux(maindir = "path/to/data", measure = "measure", branch = "branch"), NA)
  expect_equal(pcn_region_code, "SSA")
  expect_true(is.data.frame(load_aux(maindir = "path/to/data", measure = "measure", branch = "branch")))
})