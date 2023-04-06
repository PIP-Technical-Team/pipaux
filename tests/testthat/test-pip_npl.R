# load data using `pip_npl()` from DEV branch
npl_data <- pip_npl(action = "load",
                        branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_npl(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
npl_update_data <- pip_npl(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP national poverty headcount data", {

  expect_equal(any(duplicated(npl_data,
                              by = c("country_code", "reporting_year"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    npl_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, reporting_year`", {

  pointblank::expect_rows_complete(
    npl_data,
    columns = c("country_code", "reporting_year"),
    threshold = 1
  )
})
test_that("Variables data types in PIP national poverty headcount data", {

  expect_true(is.character(npl_data[["country_code"]]))
  expect_true(is.numeric(npl_data[["reporting_year"]]))
  expect_true(is.numeric(npl_data[["nat_headcount"]]))
  expect_true(is.numeric(npl_data[["comparability"]]))
  expect_true(is.character(npl_data[["footnote"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(npl_data), colnames(npl_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(npl_data, npl_update_data, ignore_attr = TRUE)
})
