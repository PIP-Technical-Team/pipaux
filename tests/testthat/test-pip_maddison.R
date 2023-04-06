# load data using `pip_maddison()` from DEV branch
maddison_data <- pip_maddison(action = "load",
                                      branch = "DEV")


# generate data as action = update and save it in Testing folder
pip_maddison(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
maddison_update_data <- pip_maddison(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP maddison data", {
  expect_equal(any(duplicated(maddison_data,
                              by = c("country_code", "year"))),
               FALSE)
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    maddison_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `country_code, year`", {

  pointblank::expect_rows_complete(
    maddison_data,
    columns = c("country_code", "year"),
    threshold = 1
  )
})

test_that("Variables data types in PIP maddison data", {
  expect_true(is.character(maddison_data[["country_code"]]))
  expect_true(is.numeric(maddison_data[["year"]]))
  expect_true(is.numeric(maddison_data[["mpd_gdp"]]))
})

test_that("Variable labels in PIP maddison data", {
  expect_equal(attr(maddison_data[["country_code"]], "label"),
               "Country code")
  expect_equal(attr(maddison_data[["year"]], "label"),
               "Year")
  expect_equal(attr(maddison_data[["mpd_gdp"]], "label"),
              "GDP per capita in 2011US$, 2011 benchmark (Maddison)")
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(maddison_data), colnames(maddison_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(maddison_data, maddison_update_data, ignore_attr = TRUE)
})
