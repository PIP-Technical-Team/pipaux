# load data using `pip_gdp()` from DEV branch
gdp_data <- pip_gdp(action = "load",
                    branch = "DEV")
gdp_data_unq <- unique(gdp_data,
                       by = c("country_code", "year",
                            "gdp_data_level"))

test_that("No duplicate records in PIP GDP data", {
  # skip_if_offline()
  expect_identical(gdp_data, gdp_data_unq)
})

test_that("Key variables data types in PIP GDP data", {
  # skip_if_offline()
  expect_true(is.character(gdp_data[["country_code"]]))
  expect_true(is.numeric(gdp_data[["year"]]))
  expect_true(is.character(gdp_data[["gdp_data_level"]]))

  # other variable
  expect_true(is.numeric(gdp_data[["gdp"]]))
})
