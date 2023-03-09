test_that("No duplicate records in PIP PPP data", {
  # skip_if_offline()
  ppp_data <- pip_ppp(action = "load",
                      branch = "DEV")
  ppp_data_unq <- unique(ppp_data,
                         by=c("country_code", "ppp_year",
                              "release_version", "adaptation_version",
                              "ppp_data_level"))
  expect_identical(ppp_data, ppp_data_unq)
})

test_that("Key variables data types in PIP PPP data", {
  # skip_if_offline()
  ppp_data <- pip_ppp(action = "load",
                      branch = "DEV")

  expect_true(is.character(ppp_data[["country_code"]]))
  expect_true(is.numeric(ppp_data[["ppp_year"]]))
  expect_true(is.character(ppp_data[["release_version"]]))
  expect_true(is.character(ppp_data[["adaptation_version"]]))
  expect_true(is.character(ppp_data[["ppp_data_level"]]))

  # other variables
  expect_true(is.numeric(ppp_data[["ppp"]]))
  expect_true(is.logical(ppp_data[["ppp_default"]]))
  expect_true(is.logical(ppp_data[["ppp_default_by_year"]]))
  expect_true(is.character(ppp_data[["ppp_domain"]]))
})
