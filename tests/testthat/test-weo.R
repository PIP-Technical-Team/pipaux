# helper: valid raw weo data ----------------------------------------------

make_weo_raw <- function(...) {
  dt <- data.table::data.table(
    `WEO Country Code`             = c("111", "112"),
    ISO                            = c("AAA", "BBB"),
    `WEO Subject Code`             = c("NGDPRPC", "NGDPRPPPPC"),
    Country                        = c("Country A", "Country B"),
    `Subject Descriptor`           = c("GDP", "GDP PPP"),
    `Subject Notes`                = c("", ""),
    Units                          = c("LCU", "PPP"),
    Scale                          = c("Billions", "Billions"),
    `Country/Series-specific Notes` = c("", ""),
    `Estimates Start After`        = c(2010, 2011),
    `2010`                         = c(1000.5, 2000.5),
    `2011`                         = c(1100.5, 2100.5)
  )
  modifyList(dt, list(...))
}

# helper: valid output weo data -------------------------------------------

make_weo_output <- function(...) {
  dt <- data.table::data.table(
    country_code = c("AAA", "BBB"),
    year         = c(2010, 2011),
    weo_gdp      = c(1000.5, 2000.5)
  )
  modifyList(dt, list(...))
}

# weo_validate_raw() ------------------------------------------------------

test_that("weo_validate_raw() passes with valid data", {
  expect_no_error(weo_validate_raw(weo = make_weo_raw(), detail = FALSE))
})

test_that("weo_validate_raw() errors when ISO is not character", {
  bad <- make_weo_raw(ISO = c(1, 2))
  expect_error(weo_validate_raw(weo = bad, detail = FALSE))
})

test_that("weo_validate_raw() errors when WEO Subject Code is not character", {
  bad <- make_weo_raw(`WEO Subject Code` = c(1, 2))
  expect_error(weo_validate_raw(weo = bad, detail = FALSE))
})

test_that("weo_validate_raw() errors when Country is not character", {
  bad <- make_weo_raw(Country = c(1, 2))
  expect_error(weo_validate_raw(weo = bad, detail = FALSE))
})

test_that("weo_validate_raw() errors when Estimates Start After is not numeric", {
  bad <- make_weo_raw(`Estimates Start After` = c("a", "b"))
  expect_error(weo_validate_raw(weo = bad, detail = FALSE))
})

test_that("weo_validate_raw() errors when ISO is NA", {
  bad <- make_weo_raw(ISO = c("AAA", NA_character_))
  expect_error(weo_validate_raw(weo = bad, detail = FALSE))
})

test_that("weo_validate_raw() errors on duplicate key values", {
  bad <- make_weo_raw(
    ISO                  = c("AAA", "AAA"),
    `WEO Subject Code`   = c("NGDPRPC", "NGDPRPC")
  )
  expect_error(weo_validate_raw(weo = bad, detail = FALSE))
})

test_that("weo_validate_raw() errors when data is NULL", {
  expect_error(weo_validate_raw(weo = NULL, detail = FALSE))
})

# weo_validate_output() ---------------------------------------------------

test_that("weo_validate_output() passes with valid data", {
  expect_no_error(weo_validate_output(weo = make_weo_output(), detail = FALSE))
})

test_that("weo_validate_output() errors when country_code is not character", {
  bad <- make_weo_output(country_code = c(1, 2))
  expect_error(weo_validate_output(weo = bad, detail = FALSE))
})

test_that("weo_validate_output() errors when year is not numeric", {
  bad <- make_weo_output(year = c("2010", "2011"))
  expect_error(weo_validate_output(weo = bad, detail = FALSE))
})

test_that("weo_validate_output() errors when weo_gdp is not numeric", {
  bad <- make_weo_output(weo_gdp = c("a", "b"))
  expect_error(weo_validate_output(weo = bad, detail = FALSE))
})

test_that("weo_validate_output() errors when country_code is NA", {
  bad <- make_weo_output(country_code = c("AAA", NA_character_))
  expect_error(weo_validate_output(weo = bad, detail = FALSE))
})

test_that("weo_validate_output() errors when year is NA", {
  bad <- make_weo_output(year = c(2010, NA_real_))
  expect_error(weo_validate_output(weo = bad, detail = FALSE))
})

test_that("weo_validate_output() errors on duplicate key values", {
  bad <- make_weo_output(
    country_code = c("AAA", "AAA"),
    year         = c(2010, 2010)
  )
  expect_error(weo_validate_output(weo = bad, detail = FALSE))
})

test_that("weo_validate_output() errors when data is NULL", {
  expect_error(weo_validate_output(weo = NULL, detail = FALSE))
})

# aux_weo() / aux_weo_clean() ---------------------------------------------

test_that("aux_weo() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

