# load data using `pip_pce()` from DEV branch
pip_pce_data <- pip_pce(action = "load",
                        branch = "DEV")

test_that("No duplicate records in PIP PCE data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_pce_data,
                              by = c("country_code", "year",
                                     "pce_data_level"))), FALSE)
})

test_that("Variables data types in PIP PCE data", {
  # skip_if_offline()
  expect_true(is.character(pip_pce_data[["country_code"]]))
  expect_true(is.character(pip_pce_data[["pce_data_level"]]))
  expect_true(is.character(pip_pce_data[["pce_domain"]]))
  expect_true(is.numeric(pip_pce_data[["year"]]))
  expect_true(is.numeric(pip_pce_data[["pce"]]))
})
