# load data using `pip_pl()` from DEV branch
pip_pl_data <- pip_pl(action = "load",
                      branch = "DEV")

test_that("No duplicate records in PIP Poverty line data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_pl_data,
                              by = c("ppp_year", "poverty_line"))), FALSE)
})

test_that("Variables data types in PIP Poverty line data", {
  # skip_if_offline()
  expect_true(is.character(pip_pl_data[["name"]]))
  expect_true(is.numeric(pip_pl_data[["poverty_line"]]))
  expect_true(is.logical(pip_pl_data[["is_default"]]))
  expect_true(is.logical(pip_pl_data[["is_visible"]]))
  expect_true(is.numeric(pip_pl_data[["ppp_year"]]))
})
