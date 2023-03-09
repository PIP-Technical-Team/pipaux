test_that("No duplicate records in PIP PFW data", {
  # skip_if_offline()
  pip_pfw <- pip_pfw(action = "load",
                      branch = "DEV")
  pip_pfw_unq <- unique(pip_pfw,
                         by=c("country_code", "year", "survey_acronym"))
  expect_identical(pip_pfw, pip_pfw_unq)
})

test_that("Key variables data types in PIP PFW data", {
  # skip_if_offline()
  pip_pfw <- pip_pfw(action = "load",
                      branch = "DEV")

  expect_true(is.character(pip_pfw[["country_code"]]))
  expect_true(is.numeric(pip_pfw[["year"]]))
  expect_true(is.character(pip_pfw[["survey_acronym"]]))
})
