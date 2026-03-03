library(testthat)

test_that("maddison_validate_output() works correctly", {
  expect_error(maddison_validate_output(), "missing value")
  expect_true(maddison_validate_output(data = valid_data))
  expect_false(maddison_validate_output(data = invalid_data))
})