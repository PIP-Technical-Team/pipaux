# load data using `pip_pl()` from DEV branch
pl_data <- pip_pl(action = "load",
                      branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_pl(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
pl_update_data <- pip_pl(action = "load",
                           maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                           branch = "DEV")

test_that("No duplicate records in PIP Poverty line data", {

  expect_equal(any(duplicated(pl_data,
                              by = c("name", "ppp_year"))), FALSE)
})

test_that("complete rows across `ppp_year, ppp_year`", {

  pointblank::expect_rows_complete(
    pl_data,
    columns = c("name", "ppp_year"),
    threshold = 1
  )
})

test_that("Variables data types in PIP Poverty line data", {

  expect_true(is.character(pl_data[["name"]]))
  expect_true(is.numeric(pl_data[["poverty_line"]]))
  expect_true(is.logical(pl_data[["is_default"]]))
  expect_true(is.logical(pl_data[["is_visible"]]))
  expect_true(is.numeric(pl_data[["ppp_year"]]))
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(pl_data), colnames(pl_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(pl_data, pl_update_data, ignore_attr = TRUE)
})
