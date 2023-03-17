# load data using `pip_missing_data()` from DEV branch
pip_missingdata_data <- pip_missing_data(action = "load",
                                  branch = "DEV")

test_that("No duplicate records in PIP missing data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_missingdata_data,
                              by = c("country_code", "year"))),
               FALSE)
})

test_that("Variables data types in PIP metadata data", {
  # skip_if_offline()
  expect_true(is.character(pip_missingdata_data[["country_code"]]))
  expect_true(is.numeric(pip_missingdata_data[["year"]]))
  expect_true(is.numeric(pip_missingdata_data[["reporting_pop"]]))
  expect_true(is.character(pip_missingdata_data[["region_code"]]))
})
