# load data using `pip_gdp()` from DEV branch
gdp_data <- pip_gdp(action = "load",
                    branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_gdp(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
gdp_update_data <- pip_gdp(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP GDP data", {

  expect_equal(any(duplicated(gdp_data,
                              by = c("country_code", "year",
                                     "gdp_data_level"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    gdp_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, year, gdp_data_level`", {

  pointblank::expect_rows_complete(
    gdp_data,
    columns = c("country_code", "year", "gdp_data_level"),
    threshold = 1
  )
})

test_that("Key variables data types in PIP GDP data", {

  expect_true(is.character(gdp_data[["country_code"]]))
  expect_true(is.numeric(gdp_data[["year"]]))
  expect_true(is.character(gdp_data[["gdp_data_level"]]))

  expect_true(is.numeric(gdp_data[["gdp"]]))
})

test_that("Variable labels in PIP GDP data", {

  expect_equal(attr(gdp_data[["country_code"]], "label"),
               "Country code")
  expect_equal(attr(gdp_data[["year"]], "label"),
               "Year")
  expect_equal(attr(gdp_data[["gdp_data_level"]], "label"),
               "Values to use as keys to join with gdp_domain_var")
  expect_equal(attr(gdp_data[["gdp"]], "label"),
               "GDP per capita (constant 2010 US$)")
  expect_equal(attr(gdp_data[["gdp_domain"]], "label"),
               "GDP domain to join with microdata")
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(gdp_data), colnames(gdp_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(gdp_data, gdp_update_data, ignore_attr = TRUE)
})
