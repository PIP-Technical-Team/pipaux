# load data using `pip_pfw()` from DEV branch
pip_pfw_data <- pip_pfw(action = "load",
                   branch = "DEV")

test_that("No duplicate records in PIP PFW data", {
  # skip_if_offline()
  expect_equal(any(duplicated(pip_pfw_data,
                              by = c("country_code", "year", "survey_acronym"))),
               FALSE)
})

test_that("Key variables data types in PIP PFW data", {
  # skip_if_offline()
  expect_true(is.character(pip_pfw_data[["country_code"]]))
  expect_true(is.numeric(pip_pfw_data[["year"]]))
  expect_true(is.character(pip_pfw_data[["survey_acronym"]]))
})
