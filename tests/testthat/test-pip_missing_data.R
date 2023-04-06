# load data using `pip_missing_data()` from DEV branch
missingdata_data <- pip_missing_data(action = "load",
                                  branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_missing_data(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
missingdata_update_data <- pip_missing_data(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP missing data", {

  expect_equal(any(duplicated(missingdata_data,
                              by = c("country_code", "year"))),
               FALSE)
})

test_that("complete rows across `country_code`", {

  pointblank::expect_rows_complete(
    missingdata_data,
    columns = c("country_code", "year"),
    threshold = 1
  )
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    missingdata_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("Variables data types in PIP metadata data", {

  expect_true(is.character(missingdata_data[["country_code"]]))
  expect_true(is.numeric(missingdata_data[["year"]]))
  expect_true(is.numeric(missingdata_data[["reporting_pop"]]))
  expect_true(is.character(missingdata_data[["region_code"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(missingdata_data), colnames(missingdata_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(missingdata_data, missingdata_update_data, ignore_attr = TRUE)

})
