library(testthat)

test_that("income groups validation works correctly", {
  # Example test case for income groups validation
  result <- income_groups_validation(data)
  expected <- TRUE  # Replace with actual expected result
  expect_equal(result, expected)
})