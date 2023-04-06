# load data using `pip_countries()` from DEV branch
countries_data <- pip_countries(action = "load",
                                branch = "DEV")

countries_data_unq <- unique(countries_data,
                           by = "country_code")

# generate data as action = update and save it in Testing folder
pip_countries(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
countries_update_data <- pip_countries(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

pfw <- load_aux(measure = "pfw",
                maindir = gls$PIP_DATA_DIR,
                branch  = "DEV")

pfw_country <- pfw[, .(country_code)][order(country_code)] |> unique()


test_that("No duplicate records in PIP country list data", {

  expect_equal(any(duplicated(countries_data,
                              by = "country_code")),
               FALSE)
})

test_that("Correct country ISO code", {

  expect_true(all(countries_data_unq[, .(country_code)] %in%
                    pfw_country[, .(country_code)]))
})

test_that("complete rows across `country_code`", {

  pointblank::expect_rows_complete(
    countries_data,
    columns = "country_code",
    threshold = 1
  )
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    countries_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("values in `region_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    countries_data[["region_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("Key variables data types in PIP countries list data", {

  #expect_true(sapply(colnames(countries_data), function(x) is.character(x)))
  expect_true(is.character(countries_data[["country_code"]]))

  # other variables
  expect_true(is.character(countries_data[["country_name"]]))
  expect_true(is.character(countries_data[["africa_split"]]))
  expect_true(is.character(countries_data[["africa_split_code"]]))
  expect_true(is.character(countries_data[["region"]]))
  expect_true(is.character(countries_data[["region_code"]]))
  expect_true(is.character(countries_data[["world"]]))
  expect_true(is.character(countries_data[["world_code"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(countries_data), colnames(countries_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(countries_data, countries_update_data, ignore_attr = TRUE)

})
