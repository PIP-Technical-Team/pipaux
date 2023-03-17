# load data using `pip_country_list()` from DEV branch
country_list_data <- pip_country_list(action = "load",
                                branch = "DEV")

test_that("No duplicate records in PIP country list data", {
  # skip_if_offline()
  expect_equal(any(duplicated(country_list_data,
                              by = "country_code")),
               FALSE)
})


test_that("Key variables data types in PIP countries list data", {
  # skip_if_offline()
  expect_true(is.character(country_list_data[["country_code"]]))

  # other variables
  expect_true(is.character(country_list_data[["country_name"]]))
  expect_true(is.character(country_list_data[["africa_split"]]))
  expect_true(is.character(country_list_data[["africa_split_code"]]))
  expect_true(is.character(country_list_data[["pcn_region"]]))
  expect_true(is.character(country_list_data[["pcn_region_code"]]))
  expect_true(is.character(country_list_data[["region"]]))
  expect_true(is.character(country_list_data[["region_code"]]))
  expect_true(is.character(country_list_data[["world"]]))
  expect_true(is.character(country_list_data[["world_code"]]))
})
