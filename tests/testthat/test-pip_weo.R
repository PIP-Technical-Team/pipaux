# load data using `pip_weo()` from DEV branch
weo_data <- pip_weo(action = "load",
                    branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_weo(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
weo_update_data <- pip_weo(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in WEO GDP data", {

  expect_equal(any(duplicated(weo_data,
                              by = c("country_code", "year"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    weo_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, year`", {

  pointblank::expect_rows_complete(
    weo_data,
    columns = c("country_code", "year"),
    threshold = 1
  )
})

test_that("Key variables data types in WEO GDP data", {

  expect_true(is.character(weo_data[["country_code"]]))
  expect_true(is.numeric(weo_data[["year"]]))

  # other variable
  expect_true(is.numeric(weo_data[["weo_gdp"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(weo_data), colnames(weo_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(weo_data, weo_update_data, ignore_attr = TRUE)
})
