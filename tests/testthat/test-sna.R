# helper: valid raw sna data ----------------------------------------------

make_sna_raw <- function(...) {
  dt <- data.table::data.table(
    countryname = c("Country A", "Country B"),
    coverage    = c("National", "National"),
    countrycode = c("AAA", "BBB"),
    year        = c(2010, 2011),
    GDP         = c(1000.5, 2000.5),
    PCE         = c(TRUE, FALSE),
    sourceGDP   = c("WDI", "WDI"),
    sourcePCE   = c(TRUE, FALSE)
  )
  modifyList(dt, list(...))
}

# helper: valid raw sna_fy data -------------------------------------------

make_sna_fy_raw <- function(...) {
  dt <- data.table::data.table(
    Code         = c("AAA", "BBB"),
    LongName     = c("Country A", "Country B"),
    SpecialNotes = c("", ""),
    Month        = c("January", "June"),
    Day          = c(1, 30)
  )
  modifyList(dt, list(...))
}

# sna_validate_raw() ------------------------------------------------------


test_that("sna_validate_raw() errors when coverage has invalid value", {
  bad <- make_sna_raw(coverage = c("National", "Invalid"))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when countrycode is not character", {
  bad <- make_sna_raw(countrycode = c(1, 2))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when year is not numeric", {
  bad <- make_sna_raw(year = c("2010", "2011"))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when GDP is not numeric", {
  bad <- make_sna_raw(GDP = c("a", "b"))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when PCE is not logical", {
  bad <- make_sna_raw(PCE = c(1, 0))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when sourcePCE is not logical", {
  bad <- make_sna_raw(sourcePCE = c(1, 0))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when countrycode is NA", {
  bad <- make_sna_raw(countrycode = c("AAA", NA_character_))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when year is NA", {
  bad <- make_sna_raw(year = c(2010, NA_real_))
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors on duplicate key values", {
  bad <- make_sna_raw(
    countrycode = c("AAA", "AAA"),
    year        = c(2010, 2010)
  )
  expect_error(sna_validate_raw(sna = bad, detail = FALSE))
})

test_that("sna_validate_raw() errors when data is NULL", {
  expect_error(sna_validate_raw(sna = NULL, detail = FALSE))
})

# sna_fy_validate_raw() ---------------------------------------------------

test_that("sna_fy_validate_raw() errors when Code is not character", {
  bad <- make_sna_fy_raw(Code = c(1, 2))
  expect_error(sna_fy_validate_raw(sna_fy = bad, detail = FALSE))
})

test_that("sna_fy_validate_raw() errors when Month is not character", {
  bad <- make_sna_fy_raw(Month = c(1, 6))
  expect_error(sna_fy_validate_raw(sna_fy = bad, detail = FALSE))
})

test_that("sna_fy_validate_raw() errors when Day is not numeric", {
  bad <- make_sna_fy_raw(Day = c("a", "b"))
  expect_error(sna_fy_validate_raw(sna_fy = bad, detail = FALSE))
})

test_that("sna_fy_validate_raw() errors when Code is NA", {
  bad <- make_sna_fy_raw(Code = c("AAA", NA_character_))
  expect_error(sna_fy_validate_raw(sna_fy = bad, detail = FALSE))
})

test_that("sna_fy_validate_raw() errors when Month is NA", {
  bad <- make_sna_fy_raw(Month = c("January", NA_character_))
  expect_error(sna_fy_validate_raw(sna_fy = bad, detail = FALSE))
})

test_that("sna_fy_validate_raw() errors when Day is NA", {
  bad <- make_sna_fy_raw(Day = c(1, NA_real_))
  expect_error(sna_fy_validate_raw(sna_fy = bad, detail = FALSE))
})

test_that("sna_fy_validate_raw() errors when data is NULL", {
  expect_error(sna_fy_validate_raw(sna_fy = NULL, detail = FALSE))
})

# aux_sna() ---------------------------------------------------------------

test_that("aux_sna() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

test_that("sna_validate_output() works", {
  skip("not yet implemented")
})

