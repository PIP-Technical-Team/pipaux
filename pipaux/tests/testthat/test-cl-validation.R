library(testthat)

test_that("cl_validate_raw() works identifying invalid value", {
  expect_error(cl_validate_raw(pcn_region_code = "invalid"), "object 'pcn_region_code' not found")
})

test_that("cl_validate_raw() works identifying duplicate error", {
  expect_error(cl_validate_raw(pcn_region_code = "SSA"), "could not find function 'load_aux'")
})

test_that("cl_validate_raw() works identifying valid value", {
  result <- cl_validate_raw(pcn_region_code = "SAR")
  expect_true(result)
})