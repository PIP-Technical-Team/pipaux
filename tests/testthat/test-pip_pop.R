# load data using `pip_pop()` from DEV branch
pop_data <- pip_pop(action = "load",
                        branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_pop(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
pop_update_data <- pip_pop(action = "load",
                        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                        branch = "DEV")

test_that("No duplicate records in PIP pop data", {

  expect_equal(any(duplicated(pop_data,
                              by = c("country_code", "year",
                                     "pop_data_level"))), FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    pop_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, year, pop_data_level`", {

  pointblank::expect_rows_complete(
    pop_data,
    columns = c("country_code", "year", "pop_data_level"),
    threshold = 1
  )
})

test_that("Variables data types in PIP pop data", {

  expect_true(is.character(pop_data[["country_code"]]))
  expect_true(is.numeric(pop_data[["year"]]))
  expect_true(is.character(pop_data[["pop_data_level"]]))
  expect_true(is.numeric(pop_data[["pop"]]))
  expect_true(is.character(pop_data[["pop_domain"]]))
})

test_that("Variable labels in PIP pop data", {

  expect_equal(attr(pop_data[["country_code"]], "label"),
               "Country code")
  expect_equal(attr(pop_data[["year"]], "label"),
               "Year")
  expect_equal(attr(pop_data[["pop_data_level"]], "label"),
               "Values to use as keys to join with pop_domain_var")
  expect_equal(attr(pop_data[["pop"]], "label"),
               "Population")
  expect_equal(attr(pop_data[["pop_domain"]], "label"),
               "Population domain to join with microdata")
})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(pop_data, pop_update_data, ignore_attr = TRUE)
})

