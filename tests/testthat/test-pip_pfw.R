# load data using `pip_pfw()` from DEV branch
pip_pfw <- pip_pfw(action = "load",
                   branch = "DEV")

pip_pfw_unq <- unique(pip_pfw,
                      by = c("country_code", "year", "survey_acronym"))

test_that("No duplicate records in PIP PFW data", {
  # skip_if_offline()
  expect_identical(pip_pfw, pip_pfw_unq)
})

test_that("Key variables data types in PIP PFW data", {
  # skip_if_offline()
  expect_true(is.character(pip_pfw[["country_code"]]))
  expect_true(is.numeric(pip_pfw[["year"]]))
  expect_true(is.character(pip_pfw[["survey_acronym"]]))
})
