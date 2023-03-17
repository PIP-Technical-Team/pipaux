# load data using `pip_maddison()` from DEV branch
pip_maddison_data <- pip_maddison(action = "load",
                                      branch = "DEV")

test_that("No duplicate records in PIP maddison data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_maddison_data,
                              by = c("country_code", "year"))),
               FALSE)
})

test_that("Variables data types in PIP maddison data", {
  # skip_if_offline()
  expect_true(is.character(pip_maddison_data[["country_code"]]))
  expect_true(is.numeric(pip_maddison_data[["year"]]))
  expect_true(is.numeric(pip_maddison_data[["mpd_gdp"]]))
})
