# load data using `pip_countries()` from DEV branch
countries_data <- pip_countries(action = "load",
                                branch = "DEV")

countries_data_unq <- unique(countries_data,
                           by = "country_code")

pfw <- load_aux(measure = "pfw",
                maindir = gls$PIP_DATA_DIR,
                branch  = "DEV")

pfw_country <- pfw[, .(country_code)][order(country_code)] |> unique()


test_that("No duplicate records in PIP country list data", {
  # skip_if_offline()
  expect_equal(any(duplicated(countries_data,
                              by = "country_code")),
               FALSE)
})

test_that("Correct country ISO code", {
  # skip_if_offline()
  expect_true(all(countries_data_unq[, .(country_code)] %in%
                    pfw_country[, .(country_code)]))
})

test_that("Key variables data types in PIP countries list data", {
  # skip_if_offline()
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

