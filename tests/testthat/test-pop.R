# helper: valid pop main raw data -----------------------------------------

make_popmain_raw <- function(...) {
  dt <- data.table::data.table(
    country_code   = c("AAA", "BBB"),
    year           = c(2010, 2011),
    pop_data_level = c(0, 2),
    pop            = c(1000000, 2000000)
  )
  modifyList(dt, list(...))
}

# helper: valid WDI pop raw data ------------------------------------------

make_pop_raw <- function(...) {
  dt <- data.table::data.table(
    indicator_id  = c("SP.POP.TOTL", "SP.RUR.TOTL"),
    indicator     = c("Population, total", "Rural population"),
    iso2c         = c("AA", "BB"),
    iso3c         = c("AAA", "BBB"),
    country       = c("Country A", "Country B"),
    date          = c(2010, 2011),
    value         = c(1000000, 2000000),
    unit          = c("", ""),
    obs_status    = c("", ""),
    footnote      = c("", ""),
    last_updated  = as.Date(c("2023-01-01", "2023-01-01"))
  )
  modifyList(dt, list(...))
}

# helper: valid special cases pop raw data --------------------------------

make_spop_raw <- function(...) {
  dt <- data.table::data.table(
    country_code   = c("AAA", "BBB"),
    year           = c(2010, 2011),
    pop_data_level = c(0, 2),
    pop            = c(500000, 1500000)
  )
  modifyList(dt, list(...))
}

# helper: valid output pop data -------------------------------------------

make_pop_output <- function(...) {
  dt <- data.table::data.table(
    country_code    = c("AAA", "BBB"),
    year            = c(2010, 2011),
    reporting_level = c("national", "rural"),
    pop             = c(1000000, 2000000)
  )
  modifyList(dt, list(...))
}

# popmain_validate_raw() --------------------------------------------------

test_that("popmain_validate_raw() passes with valid data", {
  expect_no_error(popmain_validate_raw(pop_main = make_popmain_raw(), detail = FALSE))
})

test_that("popmain_validate_raw() errors when pop_data_level has invalid value", {
  bad <- make_popmain_raw(pop_data_level = c(0, 99))
  expect_error(popmain_validate_raw(pop_main = bad, detail = FALSE))
})

test_that("popmain_validate_raw() errors when country_code is NA", {
  bad <- make_popmain_raw(country_code = c("AAA", NA_character_))
  expect_error(popmain_validate_raw(pop_main = bad, detail = FALSE))
})

test_that("popmain_validate_raw() errors when year is NA", {
  bad <- make_popmain_raw(year = c(2010, NA_real_))
  expect_error(popmain_validate_raw(pop_main = bad, detail = FALSE))
})

test_that("popmain_validate_raw() errors when pop is not numeric", {
  bad <- make_popmain_raw(pop = c("a", "b"))
  expect_error(popmain_validate_raw(pop_main = bad, detail = FALSE))
})

test_that("popmain_validate_raw() errors on duplicate key values", {
  bad <- make_popmain_raw(
    country_code   = c("AAA", "AAA"),
    year           = c(2010, 2010),
    pop_data_level = c(0, 0)
  )
  expect_error(popmain_validate_raw(pop_main = bad, detail = FALSE))
})

test_that("popmain_validate_raw() errors when data is NULL", {
  expect_error(popmain_validate_raw(pop_main = NULL, detail = FALSE))
})

# pop_validate_raw() ------------------------------------------------------

test_that("pop_validate_raw() passes with valid data", {
  expect_no_error(pop_validate_raw(pop = make_pop_raw(), detail = FALSE))
})

test_that("pop_validate_raw() errors when indicator_id has invalid value", {
  bad <- make_pop_raw(indicator_id = c("SP.POP.TOTL", "INVALID"))
  expect_error(pop_validate_raw(pop = bad, detail = FALSE))
})

test_that("pop_validate_raw() errors when indicator_id is NA", {
  bad <- make_pop_raw(indicator_id = c("SP.POP.TOTL", NA_character_))
  expect_error(pop_validate_raw(pop = bad, detail = FALSE))
})

test_that("pop_validate_raw() errors when iso3c is NA", {
  bad <- make_pop_raw(iso3c = c("AAA", NA_character_))
  expect_error(pop_validate_raw(pop = bad, detail = FALSE))
})

test_that("pop_validate_raw() errors when date is NA", {
  bad <- make_pop_raw(date = c(2010, NA_real_))
  expect_error(pop_validate_raw(pop = bad, detail = FALSE))
})

test_that("pop_validate_raw() errors when date is not numeric", {
  bad <- make_pop_raw(date = c("2010", "2011"))
  expect_error(pop_validate_raw(pop = bad, detail = FALSE))
})

test_that("pop_validate_raw() errors on duplicate key values", {
  bad <- make_pop_raw(
    indicator_id = c("SP.POP.TOTL", "SP.POP.TOTL"),
    iso3c        = c("AAA", "AAA"),
    date         = c(2010, 2010)
  )
  expect_error(pop_validate_raw(pop = bad, detail = FALSE))
})

test_that("pop_validate_raw() errors when data is NULL", {
  expect_error(pop_validate_raw(pop = NULL, detail = FALSE))
})

# spop_validate_raw() -----------------------------------------------------

test_that("spop_validate_raw() passes with valid data", {
  expect_no_error(spop_validate_raw(spop = make_spop_raw(), detail = FALSE))
})

test_that("spop_validate_raw() errors when pop_data_level has invalid value", {
  bad <- make_spop_raw(pop_data_level = c(0, 99))
  expect_error(spop_validate_raw(spop = bad, detail = FALSE))
})

test_that("spop_validate_raw() errors when country_code is NA", {
  bad <- make_spop_raw(country_code = c("AAA", NA_character_))
  expect_error(spop_validate_raw(spop = bad, detail = FALSE))
})

test_that("spop_validate_raw() errors when pop is not numeric", {
  bad <- make_spop_raw(pop = c("a", "b"))
  expect_error(spop_validate_raw(spop = bad, detail = FALSE))
})

test_that("spop_validate_raw() errors on duplicate key values", {
  bad <- make_spop_raw(
    country_code   = c("AAA", "AAA"),
    year           = c(2010, 2010),
    pop_data_level = c(0, 0)
  )
  expect_error(spop_validate_raw(spop = bad, detail = FALSE))
})

test_that("spop_validate_raw() errors when data is NULL", {
  expect_error(spop_validate_raw(spop = NULL, detail = FALSE))
})

# pop_validate_output() ---------------------------------------------------

test_that("pop_validate_output() passes with valid data", {
  expect_no_error(pop_validate_output(pop = make_pop_output(), detail = FALSE))
})

test_that("pop_validate_output() errors when reporting_level has invalid value", {
  bad <- make_pop_output(reporting_level = c("national", "invalid"))
  expect_error(pop_validate_output(pop = bad, detail = FALSE))
})

test_that("pop_validate_output() errors when country_code is NA", {
  bad <- make_pop_output(country_code = c("AAA", NA_character_))
  expect_error(pop_validate_output(pop = bad, detail = FALSE))
})

test_that("pop_validate_output() errors when year is NA", {
  bad <- make_pop_output(year = c(2010, NA_real_))
  expect_error(pop_validate_output(pop = bad, detail = FALSE))
})

test_that("pop_validate_output() errors when reporting_level is NA", {
  bad <- make_pop_output(reporting_level = c("national", NA_character_))
  expect_error(pop_validate_output(pop = bad, detail = FALSE))
})

test_that("pop_validate_output() errors when pop is not numeric", {
  bad <- make_pop_output(pop = c("a", "b"))
  expect_error(pop_validate_output(pop = bad, detail = FALSE))
})

test_that("pop_validate_output() errors on duplicate key values", {
  bad <- make_pop_output(
    country_code    = c("AAA", "AAA"),
    year            = c(2010, 2010),
    reporting_level = c("national", "national")
  )
  expect_error(pop_validate_output(pop = bad, detail = FALSE))
})

test_that("pop_validate_output() errors when data is NULL", {
  expect_error(pop_validate_output(pop = NULL, detail = FALSE))
})

# aux_pop() / aux_pop_update() --------------------------------------------

test_that("aux_pop() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

