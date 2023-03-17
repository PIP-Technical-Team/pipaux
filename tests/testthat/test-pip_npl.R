# load data using `pip_npl()` from DEV branch
pip_npl_data <- pip_npl(action = "load",
                        branch = "DEV")

test_that("No duplicate records in PIP national poverty headcount data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_npl_data,
                              by = c("country_code", "reporting_year"))),
               FALSE)
})

test_that("Variables data types in PIP national poverty headcount data", {
  # skip_if_offline()
  expect_true(is.character(pip_npl_data[["country_code"]]))
  expect_true(is.numeric(pip_npl_data[["reporting_year"]]))
  expect_true(is.numeric(pip_npl_data[["nat_headcount"]]))
  expect_true(is.numeric(pip_npl_data[["comparability"]]))
  expect_true(is.character(pip_npl_data[["footnote"]]))
})
