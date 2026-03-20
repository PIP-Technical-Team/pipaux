library(testthat)

test_that("npl_validate_output() works correctly", {
  expect_error(npl_validate_output(invalid_input), "expected error message")
  expect_equal(npl_validate_output(valid_input), expected_output)
})