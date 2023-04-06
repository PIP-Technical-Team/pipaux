# load data using `pip_metadata()` from DEV branch
metadata_data <- pip_metadata(action = "load",
                                  branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_metadata(action = "update",
             branch = "DEV",
             maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
metadata_update_data <- pip_metadata(action = "load",
                                     maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                                     branch = "DEV")

test_that("No duplicate records in PIP metadata data", {

  expect_equal(any(duplicated(metadata_data,
                              by = c("country_code", "reporting_year",
                                     "surveyid_year", "distribution_type"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    metadata_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, reporting_year, surveyid_year, distribution_type`", {

  pointblank::expect_rows_complete(
    metadata_data,
    columns = c("country_code", "reporting_year",
                   "surveyid_year", "distribution_type"),
    threshold = 1
  )
})

test_that("Variables data types in PIP metadata data", {

  expect_true(is.character(metadata_data[["country_code"]]))
  expect_true(is.character(metadata_data[["country_name"]]))
  expect_true(is.numeric(metadata_data[["reporting_year"]]))
  expect_true(is.numeric(metadata_data[["survey_year"]]))
  expect_true(is.numeric(metadata_data[["surveyid_year"]]))
  expect_true(is.character(metadata_data[["survey_title"]]))
  expect_true(is.character(metadata_data[["survey_conductor"]]))
  expect_true(is.character(metadata_data[["survey_coverage"]]))
  expect_true(is.character(metadata_data[["welfare_type"]]))
  expect_true(is.character(metadata_data[["distribution_type"]]))
  expect_true(is.list(metadata_data[["metadata"]]))
})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(metadata_data, metadata_update_data, ignore_attr = TRUE)

})


test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(metadata_data, metadata_update_data, ignore_attr = TRUE)
})
