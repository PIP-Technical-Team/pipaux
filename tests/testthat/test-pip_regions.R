# load data using `pip_regions()` from DEV branch
regions_data <- pip_regions(action = "load",
                            branch = "DEV")

test_that("No duplicate records in PIP region description data", {
  # skip_if_offline()
  expect_equal(any(duplicated(regions_data,
                              by = "region_code")),
               FALSE)
})

test_that("Key variables data types in PIP region description data", {
  # skip_if_offline()
  expect_true(is.character(regions_data[["region_code"]]))

  # other variables
  expect_true(is.character(regions_data[["region"]]))
  expect_true(is.character(regions_data[["grouping_type"]]))
})
