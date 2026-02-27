# helper: valid raw cpi data ----------------------------------------------

make_cpi_raw <- function(...) {
  dt <- data.table::data.table(
    region           = c("EAP", "ECA"),
    code             = c("AAA", "BBB"),
    countryname      = c("Country A", "Country B"),
    year             = c(2010, 2011),
    survname         = c("Survey A", "Survey B"),
    ref_year         = c(2010.5, 2011.5),
    cpi_domain       = c("National", "Urban/Rural"),
    cpi_domain_value = c(0, 1),
    cpi_domain_var   = c("dom1", "dom2"),
    cpi_data_level   = c(0, 1),
    cpi2005          = c(TRUE, FALSE),
    cpi2011          = c(100.0, 102.0),
    cpi2011_unadj    = c(100.0, 102.0),
    cpi2017          = c(110.0, 112.0),
    cpi2017_unadj    = c(110.0, 112.0),
    cpi2021          = c(115.0, 117.0),
    cpi2021_unadj    = c(115.0, 117.0),
    change_cpi2011   = c(0, 1),
    change_cpi2017   = c(0, 1),
    change_icp2011   = c(0.1, 0.2),
    change_icp2017   = c(0.1, 0.2),
    version          = c("v1", "v2"),
    comparability    = c(1, 2),
    cur_adj          = c(1.0, 1.0),
    survey_coverage  = c("national", "urban"),
    comparable       = c(1, 0),
    cpi_id           = c("id1", "id2"),
    cpi_replication  = c(1.0, 2.0)
  )
  modifyList(dt, list(...))
}

# helper: valid output cpi data -------------------------------------------

make_cpi_output <- function(...) {
  dt <- data.table::data.table(
    country_code     = c("AAA", "BBB"),
    year             = c(2010L, 2011L),
    survey_year      = c(2010.5, 2011.5),
    cpi              = c(100.0, 102.0),
    ccf              = c(1.0, 1.0),
    survey_acronym   = c("Survey A", "Survey B"),
    change_cpi2011   = c(0, 1),
    cpi_domain_value = c(0, 1),
    cpi2005          = c(TRUE, FALSE),
    cpi2011          = c(100.0, 102.0),
    cpi2011_unadj    = c(100.0, 102.0),
    cpi2017          = c(110.0, 112.0),
    cpi2017_unadj    = c(110.0, 112.0),
    reporting_level  = c("national", "urban"),
    cpi_id           = c("id1", "id2")
  )
  modifyList(dt, list(...))
}

# cpi_validate_raw() ------------------------------------------------------

test_that("cpi_validate_raw() passes with valid data", {
  expect_no_error(cpi_validate_raw(cpi = make_cpi_raw(), detail = FALSE))
})

test_that("cpi_validate_raw() errors when region has invalid value", {
  bad <- make_cpi_raw(region = c("EAP", "INVALID"))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when cpi_domain has invalid value", {
  bad <- make_cpi_raw(cpi_domain = c("National", "Invalid"))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when cpi_data_level has invalid value", {
  bad <- make_cpi_raw(cpi_data_level = c(0, 99))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when code is NA", {
  bad <- make_cpi_raw(code = c("AAA", NA_character_))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when year is NA", {
  bad <- make_cpi_raw(year = c(2010, NA_real_))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when year is not numeric", {
  bad <- make_cpi_raw(year = c("2010", "2011"))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors on duplicate key values", {
  bad <- make_cpi_raw(
    code           = c("AAA", "AAA"),
    year           = c(2010, 2010),
    survname       = c("Survey A", "Survey A"),
    cpi_data_level = c(0, 0)
  )
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when cpi2005 is not logical", {
  bad <- make_cpi_raw(cpi2005 = c(1, 0))
  expect_error(cpi_validate_raw(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_raw() errors when data is NULL", {
  expect_error(cpi_validate_raw(cpi = NULL, detail = FALSE))
})

# cpi_validate_output() ---------------------------------------------------

test_that("cpi_validate_output() passes with valid data", {
  expect_no_error(cpi_validate_output(cpi = make_cpi_output(), detail = FALSE))
})

test_that("cpi_validate_output() errors when reporting_level has invalid value", {
  bad <- make_cpi_output(reporting_level = c("national", "invalid"))
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors when change_cpi2011 has invalid value", {
  bad <- make_cpi_output(change_cpi2011 = c(0, 99))
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors when cpi_domain_value has invalid value", {
  bad <- make_cpi_output(cpi_domain_value = c(0, 99))
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors when country_code is NA", {
  bad <- make_cpi_output(country_code = c("AAA", NA_character_))
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors when year is not integer", {
  bad <- make_cpi_output(year = c(2010.5, 2011.5))
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors when cpi2005 is not logical", {
  bad <- make_cpi_output(cpi2005 = c(1, 0))
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors on duplicate key values", {
  bad <- make_cpi_output(
    country_code    = c("AAA", "AAA"),
    year            = c(2010L, 2010L),
    survey_acronym  = c("Survey A", "Survey A"),
    reporting_level = c("national", "national")
  )
  expect_error(cpi_validate_output(cpi = bad, detail = FALSE))
})

test_that("cpi_validate_output() errors when data is NULL", {
  expect_error(cpi_validate_output(cpi = NULL, detail = FALSE))
})

# aux_cpi_clean() / aux_cpi() / aux_cpi_update() --------------------------

test_that("aux_cpi_clean() requires pipload filesystem access", {
  skip("requires pipload filesystem access")
})

test_that("aux_cpi() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

