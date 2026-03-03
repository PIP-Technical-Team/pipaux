library(testthat)

test_that("pl_validate_output() works identifying invalid value", {
  expect_error(pl_validate_output(invalid_value), "expected error message")
})

test_that("pl_validate_output() works identifying duplicate error", {
  expect_error(pl_validate_output(duplicate_value), "expected error message")
})

test_that("pl_validate_output() works identifying type/formatting error", {
  expect_error(pl_validate_output(type_error_value), "expected error message")
})