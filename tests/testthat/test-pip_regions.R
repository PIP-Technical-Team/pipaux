# load data using `pip_regions()` from DEV branch
regions_data <- pip_regions(action = "load",
                            branch = "DEV")
regions_data_unq <- unique(regions_data,
                           by = "region_code")

test_that("No duplicate records in PIP region description data", {
  # skip_if_offline()
  expect_identical(regions_data, regions_data_unq)
})

test_that("Key variables data types in PIP region description data", {
  # skip_if_offline()

  expect_true(is.character(regions_data[["region_code"]]))

  # other variables
  expect_true(is.character(regions_data[["region"]]))
  expect_true(is.character(regions_data[["grouping_type"]]))
})
