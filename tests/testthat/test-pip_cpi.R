test_that("No duplicate records in PIP CPI data", {
  # skip_if_offline()
  cpi_data <- pip_cpi(action = "load",
                     branch = "DEV")
  cpi_data_unq <- unique(cpi_data,
                       by=c("country_code", "cpi_year",
                            "survey_acronym","cpi_data_level"))
  expect_identical(cpi_data, cpi_data_unq)
})

test_that("Key variables data types in PIP CPI data", {
  # skip_if_offline()
  cpi_data <- pip_cpi(action = "load",
                     branch = "DEV")

  expect_true(is.character(cpi_data[["country_code"]]))
  expect_true(is.numeric(cpi_data[["cpi_year"]]))
  expect_true(is.character(cpi_data[["cpi_data_level"]]))
  expect_true(is.character(cpi_data[["survey_acronym"]]))

  # Other variables
  expect_true(is.numeric(cpi_data[["cpi_year"]]))
  expect_true(is.numeric(cpi_data[["cpi"]]))
  expect_true(is.numeric(cpi_data[["ccf"]]))
  expect_true(is.numeric(cpi_data[["change_cpi2011"]]))
  expect_true(is.character(cpi_data[["cpi_domain"]]))
  expect_true(is.numeric(cpi_data[["cpi_domain_value"]]))
  expect_true(is.numeric(cpi_data[["cpi2017_unadj"]]))
  expect_true(is.numeric(cpi_data[["cpi2011_unadj"]]))
  expect_true(is.numeric(cpi_data[["cpi2011"]]))
  expect_true(is.numeric(cpi_data[["cpi2017"]]))
  expect_true(is.numeric(cpi_data[["cpi2011_SM22"]]))
  expect_true(is.numeric(cpi_data[["cpi2017_SM22"]]))
  expect_true(is.logical(cpi_data[["cpi2005"]]))
  expect_true(is.character(cpi_data[["cpi_id"]]))
})
