# load data using `pip_country_list()` from DEV branch
# data based on action = load
country_list_data <- pip_country_list(action = "load",
                                      branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_country_list(action = "update",
                 branch = "DEV",
                 maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
country_list_update_data <- pip_country_list(action = "load",
                                          maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                                          branch = "DEV")

test_that("No duplicate records in PIP country list data", {

  expect_equal(any(duplicated(country_list_data,
                              by = "country_code")),
               FALSE)
})

test_that("complete rows across `country_code`", {

  pointblank::expect_rows_complete(
    country_list_data,
    columns = "country_code",
    threshold = 1
  )
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    country_list_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("Key variables data types in PIP countries list data", {

  expect_true(is.character(country_list_data[["country_code"]]))

  # other variables
  expect_true(is.character(country_list_data[["country_name"]]))
  expect_true(is.character(country_list_data[["africa_split"]]))
  expect_true(is.character(country_list_data[["africa_split_code"]]))
  expect_true(is.character(country_list_data[["pcn_region"]]))
  expect_true(is.character(country_list_data[["pcn_region_code"]]))
  expect_true(is.character(country_list_data[["region"]]))
  expect_true(is.character(country_list_data[["region_code"]]))
  expect_true(is.character(country_list_data[["world"]]))
  expect_true(is.character(country_list_data[["world_code"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(country_list_data), colnames(country_list_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(country_list_data,
               country_list_update_data,
               ignore_attr = TRUE)
})

#########################################################################
# check if all variables that should be in the country list data are available in the raw dataset

# raw cpi data
country_list_raw <- pipfun::load_from_gh(measure = "country_list",
                                         branch = "DEV")

test_that("No duplicate records in PIP country list data", {

  expect_equal(any(duplicated(country_list_raw,
                              by = "country_code")),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    country_list_raw[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("values in `pcn_region_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    country_list_raw[["pcn_region_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("values in `region_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    country_list_raw[["region_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code`", {

  pointblank::expect_rows_complete(
    country_list_raw,
    columns = "country_code",
    threshold = 1
  )
})

test_that("values in `africa_split_code` should be in the set of `AFE`, `AFW`", {

  pointblank::expect_col_vals_in_set(
    country_list_raw,
    columns = "africa_split_code",
    set = c("AFE", "AFW", NA),
    threshold = 1
  )
})

