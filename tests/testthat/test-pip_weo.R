test_that("No duplicate records in WEO GDP data", {
  # skip_if_offline()
  weo_data <- pip_weo(action = "load",
                      branch = "DEV")
  weo_data_unq <- unique(weo_data,
                         by=c("country_code", "year"))
  expect_identical(weo_data, weo_data_unq)
})

test_that("Key variables data types in WEO GDP data", {
  # skip_if_offline()
  weo_data <- pip_weo(action = "load",
                      branch = "DEV")

  expect_true(is.character(weo_data[["country_code"]]))
  expect_true(is.numeric(weo_data[["year"]]))

  # other variable
  expect_true(is.numeric(weo_data[["weo_gdp"]]))
})
