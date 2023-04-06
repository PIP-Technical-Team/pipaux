# load data using `pip_pfw()` from DEV branch
pfw_data <- pip_pfw(action = "load",
                   branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_pfw(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
pfw_update_data <- pip_pfw(action = "load",
                        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                        branch = "DEV")
test_that("No duplicate records in PIP PFW data", {

  expect_equal(any(duplicated(pfw_data,
                              by = c("country_code", "year", "survey_acronym"))),
               FALSE)
})

test_that("complete rows across `country_code, year, survey_acronym`", {

  pointblank::expect_rows_complete(
    pfw_data,
    columns = c("country_code", "year", "survey_acronym"),
    threshold = 1
  )
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    pfw_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("Key variables data types in PIP PFW data", {

  expect_true(is.character(pfw_data[["country_code"]]))
  expect_true(is.numeric(pfw_data[["year"]]))
  expect_true(is.character(pfw_data[["survey_acronym"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(pfw_data), colnames(pfw_update_data))

})

### comparing data loaded from `load` and `update` arguments
test_that("Compare data generated using `load` and `update` parameters", {
  expect_true(all.equal(pfw_data, pfw_update_data, ignore_attr = TRUE))
})

