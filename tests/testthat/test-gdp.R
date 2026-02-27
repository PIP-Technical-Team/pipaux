# helper: valid output gdp data -------------------------------------------

make_gdp_output <- function(...) {
  dt <- data.table::data.table(
    country_code    = c("AAA", "BBB", "CCC"),
    year            = c(2010, 2011, 2012),
    reporting_level = c("national", "urban", "rural"),
    gdp             = c(1000.5, 2000.5, 3000.5)
  )
  modifyList(dt, list(...))
}

# gdp_validate_output() ---------------------------------------------------

test_that("gdp_validate_output() passes with valid data", {
  expect_no_error(gdp_validate_output(gdp = make_gdp_output(), detail = FALSE))
})

test_that("gdp_validate_output() errors when reporting_level has invalid value", {
  bad <- make_gdp_output(reporting_level = c("national", "urban", "invalid"))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when country_code is NA", {
  bad <- make_gdp_output(country_code = c("AAA", NA_character_, "CCC"))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when year is NA", {
  bad <- make_gdp_output(year = c(2010, NA_real_, 2012))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when reporting_level is NA", {
  bad <- make_gdp_output(reporting_level = c("national", NA_character_, "rural"))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when gdp is not numeric", {
  bad <- make_gdp_output(gdp = c("a", "b", "c"))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when country_code is not character", {
  bad <- make_gdp_output(country_code = c(1, 2, 3))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when reporting_level is not character", {
  bad <- make_gdp_output(reporting_level = c(1, 2, 3))
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors on duplicate key values", {
  bad <- make_gdp_output(
    country_code    = c("AAA", "AAA", "CCC"),
    year            = c(2010, 2010, 2012),
    reporting_level = c("national", "national", "rural")
  )
  expect_error(gdp_validate_output(gdp = bad, detail = FALSE))
})

test_that("gdp_validate_output() errors when data is NULL", {
  expect_error(gdp_validate_output(gdp = NULL, detail = FALSE))
})

# aux_gdp() / aux_gdp_update() --------------------------------------------

test_that("aux_gdp() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

