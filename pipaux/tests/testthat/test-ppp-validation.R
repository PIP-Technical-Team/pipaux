test_that("ppp_validate_output works correctly", {
  expect_error(load_aux(maindir = "temp_fld", measure = "measure", branch = "branch"), NA)
  expect_equal(ppp_validate_output(data), expected_output)
  expect_true(is.data.frame(data))
})