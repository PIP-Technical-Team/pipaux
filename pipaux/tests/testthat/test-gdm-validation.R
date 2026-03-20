library(testthat)

test_that("gdm_validate_output() works identifying duplicate error", {
  expect_error(load_aux(maindir = temp_fld, measure = measure, branch = branch), "could not find function")
})

test_that("gdm_validate_output() works identifying type/formatting error", {
  expect_error(load_aux(maindir = temp_fld, measure = measure, branch = branch), "could not find function")
})

test_that("gdm_validate_output() works identifying invalid value", {
  expect_error(load_aux(maindir = temp_fld, measure = measure, branch = branch), "could not find function")
})