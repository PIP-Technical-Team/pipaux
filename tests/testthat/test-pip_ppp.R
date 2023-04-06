# load data using `pip_ppp()` from DEV branch
ppp_data <- pip_ppp(action = "load",
                    branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_ppp(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
ppp_update_data <- pip_ppp(action = "load",
                        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                        branch = "DEV")

test_that("No duplicate records in PIP PPP data", {

  expect_equal(any(duplicated(ppp_data,
                              by = c("country_code", "ppp_year",
                                     "release_version", "adaptation_version",
                                     "ppp_data_level"))),
               FALSE)
})

test_that("complete rows across `country_code, ppp_year, release_version, adaptation_version, ppp_data_level`", {

  pointblank::expect_rows_complete(
    ppp_data,
    columns = c("country_code", "ppp_year", "release_version",
                "adaptation_version", "ppp_data_level"),
    threshold = 1
  )
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    ppp_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("Key variables data types in PIP PPP data", {

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

test_that("Variable labels in PIP PPP data", {

  expect_equal(attr(ppp_data[["ppp_domain"]], "label"),
               "PPP domain to join with microdata")
  expect_equal(attr(ppp_data[["ppp_data_level"]], "label"),
               "Values to use as keys to join with ppp_domain_var")
  ppp_year <- unique(ppp_data[ppp_data$ppp_default == TRUE, "ppp_year"])
  expect_equal(attr(ppp_data[["ppp"]], "label"),
               paste0(
                 "Purchasing Power Parity (",
                 ppp_year, "2011 ICP round)"
               ))
  expect_equal(attr(ppp_data[["ppp_year"]], "label"),
               "ICP round year ")
  expect_equal(attr(ppp_data[["release_version"]], "label"),
               "Release version of ICP round")
  expect_equal(attr(ppp_data[["adaptation_version"]], "label"),
               "Adaptation version of release")
  expect_equal(attr(ppp_data[["ppp_default"]], "label"),
               "PPP version used by default")
})

test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(ppp_data), colnames(ppp_update_data))

})

test_that("Compare data generated using `load` and `update` parameters", {
  expect_equal(ppp_data, ppp_update_data, ignore_attr = TRUE)
})

