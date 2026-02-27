# helper: valid raw wdi data ----------------------------------------------

make_wdi_raw <- function(...) {
  dt <- data.table::data.table(
    country_code       = c("AAA", "BBB"),
    year               = c(2010, 2011),
    NE.CON.PRVT.PC.KD  = c(1000.5, 2000.5),
    NY.GDP.PCAP.KD     = c(5000.5, 6000.5)
  )
  modifyList(dt, list(...))
}

# wdi_validate_raw() ------------------------------------------------------

test_that("wdi_validate_raw() passes with valid data", {
  expect_no_error(wdi_validate_raw(wdi = make_wdi_raw(), detail = FALSE))
})

test_that("wdi_validate_raw() errors when country_code is not character", {
  bad <- make_wdi_raw(country_code = c(1, 2))
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors when year is not numeric", {
  bad <- make_wdi_raw(year = c("2010", "2011"))
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors when NE.CON.PRVT.PC.KD is not numeric", {
  bad <- make_wdi_raw(NE.CON.PRVT.PC.KD = c("a", "b"))
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors when NY.GDP.PCAP.KD is not numeric", {
  bad <- make_wdi_raw(NY.GDP.PCAP.KD = c("a", "b"))
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors when country_code is NA", {
  bad <- make_wdi_raw(country_code = c("AAA", NA_character_))
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors when year is NA", {
  bad <- make_wdi_raw(year = c(2010, NA_real_))
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors on duplicate key values", {
  bad <- make_wdi_raw(
    country_code = c("AAA", "AAA"),
    year         = c(2010, 2010)
  )
  expect_error(wdi_validate_raw(wdi = bad, detail = FALSE))
})

test_that("wdi_validate_raw() errors when data is NULL", {
  expect_error(wdi_validate_raw(wdi = NULL, detail = FALSE))
})

# aux_wdi() / aux_wdi_update() --------------------------------------------

test_that("aux_wdi() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})
