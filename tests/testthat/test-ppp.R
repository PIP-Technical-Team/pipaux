# helper: valid raw ppp data ----------------------------------------------

make_ppp_raw <- function(...) {
  dt <- data.table::data.table(
    CountryName       = c("Country A", "Country B"),
    code              = c("AAA", "BBB"),
    CoverageType      = c("National", "Urban"),
    ppp_2005_v1_v1    = c(1.5, 2.5),
    ppp_2011_v1_v1    = c(1.6, 2.6),
    ppp_2011_v2_v1    = c(1.7, 2.7),
    ppp_2011_v1_v2    = c(1.8, 2.8),
    ppp_2011_v2_v2    = c(1.9, 2.9),
    ppp_2017_v1_v1    = c(2.0, 3.0),
    ppp_2017_v1_v2    = c(2.1, 3.1),
    source_ppp_2011   = c(1.0, 2.0),
    source_ppp_2005   = c(0.9, 1.9),
    datalevel         = c(0, 1),
    ppp_domain        = c(1, 2),
    ppp_domain_value  = c(1, 2),
    oldicp2005        = c(1.1, 2.1),
    oldicp2011        = c(1.2, 2.2),
    Seriesname        = c("Series A", "Series B"),
    note_may192020    = c("note1", "note2")
  )
  modifyList(dt, list(...))
}

# helper: valid output ppp data -------------------------------------------

make_ppp_output <- function(...) {
  dt <- data.table::data.table(
    country_code        = c("AAA", "BBB"),
    ppp_year            = c(2011, 2017),
    release_version     = c("v1", "v1"),
    adaptation_version  = c("v1", "v1"),
    ppp                 = c(1.5, 2.5),
    ppp_default         = c(TRUE, FALSE),
    ppp_default_by_year = c(TRUE, TRUE),
    reporting_level     = c("national", "urban")
  )
  modifyList(dt, list(...))
}

# ppp_validate_raw() ------------------------------------------------------

test_that("ppp_validate_raw() errors when CoverageType has invalid value", {
  bad <- make_ppp_raw(CoverageType = c("National", "Invalid"))
  expect_error(ppp_validate_raw(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_raw() errors when datalevel has invalid value", {
  bad <- make_ppp_raw(datalevel = c(0, 99))
  expect_error(ppp_validate_raw(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_raw() errors when ppp_domain has invalid value", {
  bad <- make_ppp_raw(ppp_domain = c(1, 99))
  expect_error(ppp_validate_raw(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_raw() errors on duplicate key values", {
  bad <- make_ppp_raw(
    code         = c("AAA", "AAA"),
    CoverageType = c("National", "National"),
    datalevel    = c(0, 0)
  )
  expect_error(ppp_validate_raw(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_raw() errors when code is NA", {
  bad <- make_ppp_raw(code = c("AAA", NA_character_))
  expect_error(ppp_validate_raw(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_raw() errors when ppp_2017_v1_v1 is not numeric", {
  bad <- make_ppp_raw(ppp_2017_v1_v1 = c("a", "b"))
  expect_error(ppp_validate_raw(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_raw() errors when data is NULL", {
  expect_error(ppp_validate_raw(ppp = NULL, detail = FALSE))
})

# ppp_validate_output() ---------------------------------------------------


test_that("ppp_validate_output() errors when reporting_level has invalid value", {
  bad <- make_ppp_output(reporting_level = c("national", "invalid"))
  expect_error(ppp_validate_output(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_output() errors on duplicate key values", {
  bad <- make_ppp_output(
    country_code       = c("AAA", "AAA"),
    ppp_year           = c(2011, 2011),
    reporting_level    = c("national", "national"),
    adaptation_version = c("v1", "v1"),
    release_version    = c("v1", "v1")
  )
  expect_error(ppp_validate_output(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_output() errors when country_code is NA", {
  bad <- make_ppp_output(country_code = c("AAA", NA_character_))
  expect_error(ppp_validate_output(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_output() errors when ppp is not numeric", {
  bad <- make_ppp_output(ppp = c("a", "b"))
  expect_error(ppp_validate_output(ppp = bad, detail = FALSE))
})

test_that("ppp_validate_output() errors when data is NULL", {
  expect_error(ppp_validate_output(ppp = NULL, detail = FALSE))
})

# aux_ppp_clean() ---------------------------------------------------------

test_that("aux_ppp_clean() returns a data.table", {
  result <- aux_ppp_clean(make_ppp_raw(), default_year = 2011)
  expect_s3_class(result, "data.table")
})

test_that("aux_ppp_clean() returns expected columns", {
  result <- aux_ppp_clean(make_ppp_raw(), default_year = 2011)
  expected_cols <- c(
    "country_code", "ppp_year", "release_version", "adaptation_version",
    "ppp", "ppp_default", "ppp_default_by_year", "ppp_domain", "ppp_data_level"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("aux_ppp_clean() removes non-WDI countries", {
  raw <- make_ppp_raw(
    code         = c("BES", "AAA"),
    CoverageType = c("National", "National"),
    datalevel    = c(0, 0),
    ppp_domain   = c(1, 1)
  )
  result <- aux_ppp_clean(raw, default_year = 2011)
  expect_false("BES" %in% result$country_code)
})

test_that("aux_ppp_clean() country_code column is character", {
  result <- aux_ppp_clean(make_ppp_raw(), default_year = 2011)
  expect_type(result$country_code, "character")
})

test_that("aux_ppp_clean() ppp_default is logical", {
  result <- aux_ppp_clean(make_ppp_raw(), default_year = 2011)
  expect_type(result$ppp_default, "logical")
})

test_that("aux_ppp_clean() ppp_year is numeric", {
  result <- aux_ppp_clean(make_ppp_raw(), default_year = 2011)
  expect_type(result$ppp_year, "double")
})

# aux_ppp() / aux_ppp_update() --------------------------------------------

test_that("aux_ppp() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})


