# load data using `pip_gdm()` from DEV branch
pip_gdm_data <- pip_gdm(action = "load",
                                      branch = "DEV")

test_that("No duplicate records in PIP gdm data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_gdm_data,
                              by = c("survey_id", "country_code",
                                     "pop_data_level"))),
               FALSE)
})

test_that("Key variables data types in PIP gdm data", {
  # skip_if_offline()
  expect_true(is.character(pip_gdm_data[["survey_id"]]))
  expect_true(is.character(pip_gdm_data[["country_code"]]))
  expect_true(is.character(pip_gdm_data[["pop_data_level"]]))

  # other variables
  expect_true(is.numeric(pip_gdm_data[["surveyid_year"]]))
  expect_true(is.numeric(pip_gdm_data[["survey_year"]]))
  expect_true(is.character(pip_gdm_data[["welfare_type"]]))
  expect_true(is.numeric(pip_gdm_data[["survey_mean_lcu"]]))
  expect_true(is.character(pip_gdm_data[["distribution_type"]]))
  expect_true(is.character(pip_gdm_data[["gd_type"]]))
  expect_true(is.character(pip_gdm_data[["pcn_source_file"]]))
  expect_true(is.character(pip_gdm_data[["pcn_survey_id"]]))
})
