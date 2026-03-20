test_that("gdp_validate_output() works correctly", {
  expect_error(gdp_validate_output(), "Error message for missing input")
  
  # Add more test cases as needed
  expect_equal(gdp_validate_output(valid_input), expected_output)
  expect_true(gdp_validate_output(another_valid_input))
})