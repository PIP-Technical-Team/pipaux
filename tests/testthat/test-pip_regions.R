# load data using `pip_regions()` from DEV branch
regions_data <- pip_regions(action = "load",
                            branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_regions(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
regions_update_data <- pip_regions(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP region description data", {

  expect_equal(any(duplicated(regions_data,
                              by = "region_code")),
               FALSE)
})

test_that("values in `region_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    regions_data[["region_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("complete rows across `region_code`", {

  pointblank::expect_rows_complete(
    regions_data,
    columns = c("region_code"),
    threshold = 1
  )
})

test_that("Key variables data types in PIP region description data", {

  expect_true(is.character(regions_data[["region_code"]]))

  # other variables
  expect_true(is.character(regions_data[["region"]]))
  expect_true(is.character(regions_data[["grouping_type"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(regions_data), colnames(regions_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(regions_data, regions_update_data, ignore_attr = TRUE)
})
