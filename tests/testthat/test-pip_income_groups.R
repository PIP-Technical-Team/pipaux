# load data using `pip_income_groups()` from DEV branch
pip_income_groups_data <- pip_income_groups(action = "load",
                        branch = "DEV")

test_that("No duplicate records in PIP income groups data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_income_groups_data,
                              by = c("country_code", "year_data"))),
               FALSE)
})


test_that("Key variables data types in PIP income groups data", {
  # skip_if_offline()
  expect_true(is.character(pip_income_groups_data[["country_code"]]))
  expect_true(is.numeric(pip_income_groups_data[["year_data"]]))

  # other variables
  expect_true(is.character(pip_income_groups_data[["incgroup_historical"]]))
  expect_true(is.character(pip_income_groups_data[["fcv_historical"]]))
  expect_true(is.character(pip_income_groups_data[["ssa_subregion_code"]]))
})
