# load data using `pip_gdm()` from DEV branch
gdm_data <- pip_gdm(action = "load",
                                      branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_gdm(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
gdm_update_data <- pip_gdm(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP gdm data", {

  expect_equal(any(duplicated(gdm_data,
                              by = c("survey_id", "country_code",
                                     "pop_data_level"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    gdm_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, survey_id, pop_data_level`", {

  pointblank::expect_rows_complete(
    gdm_data,
    columns = c("country_code", "survey_id", "pop_data_level"),
    threshold = 1
  )
})

test_that("Key variables data types in PIP gdm data", {

  expect_true(is.character(gdm_data[["survey_id"]]))
  expect_true(is.character(gdm_data[["country_code"]]))
  expect_true(is.character(gdm_data[["pop_data_level"]]))

  # other variables
  expect_true(is.numeric(gdm_data[["surveyid_year"]]))
  expect_true(is.numeric(gdm_data[["survey_year"]]))
  expect_true(is.character(gdm_data[["welfare_type"]]))
  expect_true(is.numeric(gdm_data[["survey_mean_lcu"]]))
  expect_true(is.character(gdm_data[["distribution_type"]]))
  expect_true(is.character(gdm_data[["gd_type"]]))
  expect_true(is.character(gdm_data[["pcn_source_file"]]))
  expect_true(is.character(gdm_data[["pcn_survey_id"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(gdm_data), colnames(gdm_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(gdm_data, gdm_update_data, ignore_attr = TRUE)
})

# test_that("column schemas match", {
#
#   expect_col_schema_match(
#     gdm_data,
#     schema = col_schema(
#       gdm_data,
#       survey_id = "character",
#       country_code = "character",
#       welfare_type = "character",
#       distribution_type = "character",
#       gd_type = "character",
#       surveyid_year = "integer",
#       survey_year = "numeric",
#       survey_mean_lcu = "numeric",
#       pop_data_level = "character",
#       pcn_source_file = "character",
#       pcn_survey_id = "character"
#     ),
#     threshold = 1
#   )
# })
