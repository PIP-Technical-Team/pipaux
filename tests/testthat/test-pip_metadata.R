# load data using `pip_metadata()` from DEV branch
pip_metadata_data <- pip_metadata(action = "load",
                                  branch = "DEV")

test_that("No duplicate records in PIP metadata data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_metadata_data,
                              by = c("country_code", "reporting_year",
                                     "surveyid_year", "distribution_type"))),
               FALSE)
})

test_that("Variables data types in PIP metadata data", {
  # skip_if_offline()
  expect_true(is.character(pip_metadata_data[["country_code"]]))
  expect_true(is.character(pip_metadata_data[["country_name"]]))
  expect_true(is.numeric(pip_metadata_data[["reporting_year"]]))
  expect_true(is.numeric(pip_metadata_data[["survey_year"]]))
  expect_true(is.numeric(pip_metadata_data[["surveyid_year"]]))
  expect_true(is.character(pip_metadata_data[["survey_title"]]))
  expect_true(is.character(pip_metadata_data[["survey_conductor"]]))
  expect_true(is.character(pip_metadata_data[["survey_coverage"]]))
  expect_true(is.character(pip_metadata_data[["welfare_type"]]))
  expect_true(is.character(pip_metadata_data[["distribution_type"]]))
  expect_true(is.list(pip_metadata_data[["metadata"]]))
})
