# load data using `pip_censoring()` from DEV branch
censoring_data <- pip_censoring(action = "load",
                                  branch = "DEV")


# generate data as action = update and save it in Testing folder
pip_censoring(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
censoring_update_data <- pip_censoring(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP censoring countries data", {

  expect_equal(any(duplicated(censoring_data$countries,
                              by = c("id", "statistic"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    censoring_data$countries[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `id, statistic`", {

  pointblank::expect_rows_complete(
    censoring_data$countries,
    columns = c("id", "statistic"),
    threshold = 1
  )
})

test_that("Variables data types in PIP censoring data", {

  expect_true(is.character(censoring_data$countries[["country_code"]]))
  expect_true(is.character(censoring_data$countries[["survey_acronym"]]))
  expect_true(is.numeric(censoring_data$countries[["reporting_year"]]))
  expect_true(is.character(censoring_data$countries[["reporting_level"]]))
  expect_true(is.character(censoring_data$countries[["welfare_type"]]))
  expect_true(is.character(censoring_data$countries[["statistic"]]))
  expect_true(is.character(censoring_data$countries[["id"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(censoring_data$countries),
               colnames(censoring_update_data$countries))

})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(censoring_data$countries,
               censoring_update_data$countries,
               ignore_attr = TRUE)

})
###############################################################################
test_that("No duplicate records in PIP censoring regions data", {

  expect_equal(any(duplicated(censoring_data$regions,
                              by = c("id", "statistic"))),
               FALSE)
})

test_that("values in `region_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    censoring_data$regions[["region_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `id, statistic`", {

  pointblank::expect_rows_complete(
    censoring_data$regions,
    columns = c("id", "statistic"),
    threshold = 1
  )
})

test_that("Variables data types in PIP censoring regions data", {

  expect_true(is.character(censoring_data$regions[["region_code"]]))
  expect_true(is.numeric(censoring_data$regions[["reporting_year"]]))
  expect_true(is.character(censoring_data$regions[["statistic"]]))
  expect_true(is.character(censoring_data$regions[["id"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(censoring_data$regions),
               colnames(censoring_update_data$regions))

})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(censoring_data$regions,
               censoring_update_data$regions,
               ignore_attr = TRUE)

})
