# load data using `pip_indicators()` from DEV branch
indicators_data <- pip_indicators(action = "load",
                                            branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_indicators(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
indicators_update_data <- pip_indicators(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("Variables data types in PIP indicators data", {

  expect_true(is.character(indicators_data[["page"]]))
  expect_true(is.character(indicators_data[["wdi_code"]]))
  expect_true(is.character(indicators_data[["indicator_code"]]))
  expect_true(is.character(indicators_data[["indicator_name"]]))
  expect_true(is.character(indicators_data[["scale_data"]]))
  expect_true(is.character(indicators_data[["scale_display"]]))
  expect_true(is.numeric(indicators_data[["scale_factor"]]))
  expect_true(is.numeric(indicators_data[["number_of_decimals"]]))
  expect_true(is.character(indicators_data[["indicator_definition_short"]]))
  expect_true(is.character(indicators_data[["indicator_definition_long"]]))
  expect_true(is.character(indicators_data[["key_indicator_template"]]))
  expect_true(is.character(indicators_data[["category"]]))
  expect_true(is.logical(indicators_data[["is_sensitive_to_povline"]]))
  expect_true(is.character(indicators_data[["symbol"]]))
  expect_true(is.numeric(indicators_data[["sort_order"]]))
  expect_true(is.numeric(indicators_data[["pip_sort_order"]]))
  expect_true(is.character(indicators_data[["tags"]]))
  expect_true(is.numeric(indicators_data[["from_year"]]))
  expect_true(is.numeric(indicators_data[["to_year"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(indicators_data), colnames(indicators_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(indicators_data, indicators_update_data, ignore_attr = TRUE)

})
