# load data using `pip_income_groups()` from DEV branch
income_groups_data <- pip_income_groups(action = "load",
                        branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_income_groups(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
income_groups_update_data <- pip_income_groups(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP income groups data", {

  expect_equal(any(duplicated(income_groups_data,
                              by = c("country_code", "year_data"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    income_groups_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, year_data`", {

  pointblank::expect_rows_complete(
    income_groups_data,
    columns = c("country_code", "year_data"),
    threshold = 1
  )
})

test_that("Key variables data types in PIP income groups data", {

  expect_true(is.character(income_groups_data[["country_code"]]))
  expect_true(is.numeric(income_groups_data[["year_data"]]))

  # other variables
  expect_true(is.character(income_groups_data[["incgroup_historical"]]))
  expect_true(is.character(income_groups_data[["fcv_historical"]]))
  expect_true(is.character(income_groups_data[["ssa_subregion_code"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(income_groups_data), colnames(income_groups_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(income_groups_data, income_groups_update_data, ignore_attr = TRUE)
})
