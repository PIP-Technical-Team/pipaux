library(testthat)

test_that("weo_validate_output() works correctly", {
  expect_error(weo_validate_output(NULL), "Input cannot be NULL")
  
  # Add more test cases as needed
  expect_true(TRUE)  # Placeholder for a passing test
})