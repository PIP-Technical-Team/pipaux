# helper: valid raw npl data ----------------------------------------------

make_npl_raw <- function(...) {
  dt <- data.table::data.table(
    region          = c("EAP", "ECA"),
    countrycode     = c("AAA", "BBB"),
    year            = c(2010, 2011),
    vsi_pov_nahc_nc = c(25.5, 30.0),
    vsi_pov_nahc    = c(0.255, 0.300),
    comparability   = c(1, 2),
    footnote        = c("", "")
  )
  modifyList(dt, list(...))
}

# helper: valid output npl data -------------------------------------------

make_npl_output <- function(...) {
  dt <- data.table::data.table(
    country_code  = c("AAA", "BBB"),
    year          = c(2010, 2011),
    nat_headcount = c(0.255, 0.300),
    comparability = c(1, 2),
    footnote      = c("", "")
  )
  modifyList(dt, list(...))
}

# npl_validate_raw() ------------------------------------------------------

test_that("npl_validate_raw() errors when countrycode is NA", {
  bad <- make_npl_raw(countrycode = c("AAA", NA_character_))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when year is NA", {
  bad <- make_npl_raw(year = c(2010, NA_real_))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when year is not numeric", {
  bad <- make_npl_raw(year = c("2010", "2011"))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when vsi_pov_nahc_nc is not numeric", {
  bad <- make_npl_raw(vsi_pov_nahc_nc = c("a", "b"))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when vsi_pov_nahc is not numeric", {
  bad <- make_npl_raw(vsi_pov_nahc = c("a", "b"))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when comparability is not numeric", {
  bad <- make_npl_raw(comparability = c("a", "b"))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when countrycode is not character", {
  bad <- make_npl_raw(countrycode = c(1, 2))
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors on duplicate key values", {
  bad <- make_npl_raw(
    countrycode = c("AAA", "AAA"),
    year        = c(2010, 2010)
  )
  expect_error(npl_validate_raw(npl = bad, detail = FALSE))
})

test_that("npl_validate_raw() errors when data is NULL", {
  expect_error(npl_validate_raw(npl = NULL, detail = FALSE))
})

# npl_validate_output() ---------------------------------------------------


test_that("npl_validate_output() errors when country_code is NA", {
  bad <- make_npl_output(country_code = c("AAA", NA_character_))
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors when year is NA", {
  bad <- make_npl_output(year = c(2010, NA_real_))
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors when nat_headcount is not numeric", {
  bad <- make_npl_output(nat_headcount = c("a", "b"))
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors when comparability is not numeric", {
  bad <- make_npl_output(comparability = c("a", "b"))
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors when country_code is not character", {
  bad <- make_npl_output(country_code = c(1, 2))
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors when footnote is not character", {
  bad <- make_npl_output(footnote = c(1, 2))
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors on duplicate key values", {
  bad <- make_npl_output(
    country_code = c("AAA", "AAA"),
    year         = c(2010, 2010)
  )
  expect_error(npl_validate_output(npl = bad, detail = FALSE))
})

test_that("npl_validate_output() errors when data is NULL", {
  expect_error(npl_validate_output(npl = NULL, detail = FALSE))
})

# aux_npl() ---------------------------------------------------------------

test_that("aux_npl() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

