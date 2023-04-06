# load data using `pip_pce()` from DEV branch
pce_data <- pip_pce(action = "load",
                        branch = "DEV")

# update wdi data
pip_wdi_update(branch = "DEV",
               maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# generate data as action = update and save it in Testing folder
pip_pce(action = "update",
             branch = "DEV",
             maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
pce_update_data <- pip_pce(action = "load",
                                     maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                                     branch = "DEV")

test_that("No duplicate records in PIP PCE data", {

  expect_equal(any(duplicated(pce_data,
                              by = c("country_code", "year",
                                     "pce_data_level"))), FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    pce_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, year, pce_data_level`", {

  pointblank::expect_rows_complete(
    pce_data,
    columns = c("country_code", "year", "pce_data_level"),
    threshold = 1
  )
})

test_that("Variables data types in PIP PCE data", {

  expect_true(is.character(pce_data[["country_code"]]))
  expect_true(is.character(pce_data[["pce_data_level"]]))
  expect_true(is.character(pce_data[["pce_domain"]]))
  expect_true(is.numeric(pce_data[["year"]]))
  expect_true(is.numeric(pce_data[["pce"]]))
})

test_that("Variable labels in PIP PCE data", {

  expect_equal(attr(pce_data[["country_code"]], "label"),
               "Country code")
  expect_equal(attr(pce_data[["year"]], "label"),
               "Year")
  expect_equal(attr(pce_data[["pce_data_level"]], "label"),
               "Values to use as keys to join with \n pce_domain_var in microdata")
  expect_equal(attr(pce_data[["pce"]], "label"),
               "Households and NPISHs Final consumption expenditure per capita (constant 2010 US$)")
  expect_equal(attr(pce_data[["pce_domain"]], "label"),
               "PCE domain to join with microdata")
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(pce_data), colnames(pce_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(pce_data, pce_update_data, ignore_attr = TRUE)
})
