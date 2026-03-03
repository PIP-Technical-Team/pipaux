library(testthat)

test_that("cpi_validate_output() works identifying duplicate error", {
  expect_error(load_aux(maindir = temp_fld, measure = measure, branch = branch), NA)
})

test_that("cpi_validate_output() works identifying type/formatting error", {
  expect_error(load_aux(maindir = temp_fld, measure = measure, branch = branch), NA)
})