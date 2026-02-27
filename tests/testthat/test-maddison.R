
# helper: valid raw maddison data -----------------------------------------

make_mpd_raw <- function(...) {
  dt <- data.table::data.table(
    country_code = c("AAA", "BBB"),
    year         = c(2010, 2011),
    mpd_gdp      = c(1000.5, 2000.5)
  )
  modifyList(dt, list(...))
}

# mpd_validate_raw() ------------------------------------------------------

test_that("mpd_validate_raw() passes with valid data", {
  expect_no_error(mpd_validate_raw(mpd = make_mpd_raw(), detail = FALSE))
})

test_that("mpd_validate_raw() errors when country_code is not character", {
  bad <- make_mpd_raw(country_code = c(1, 2))
  expect_error(mpd_validate_raw(mpd = bad, detail = FALSE))
})

test_that("mpd_validate_raw() errors when year is not numeric", {
  bad <- make_mpd_raw(year = c("2010", "2011"))
  expect_error(mpd_validate_raw(mpd = bad, detail = FALSE))
})

test_that("mpd_validate_raw() errors when mpd_gdp is not numeric", {
  bad <- make_mpd_raw(mpd_gdp = c("a", "b"))
  expect_error(mpd_validate_raw(mpd = bad, detail = FALSE))
})

test_that("mpd_validate_raw() errors when country_code is NA", {
  bad <- make_mpd_raw(country_code = c("AAA", NA_character_))
  expect_error(mpd_validate_raw(mpd = bad, detail = FALSE))
})

test_that("mpd_validate_raw() errors when year is NA", {
  bad <- make_mpd_raw(year = c(2010, NA_real_))
  expect_error(mpd_validate_raw(mpd = bad, detail = FALSE))
})

test_that("mpd_validate_raw() errors on duplicate key values", {
  bad <- make_mpd_raw(
    country_code = c("AAA", "AAA"),
    year         = c(2010, 2010)
  )
  expect_error(mpd_validate_raw(mpd = bad, detail = FALSE))
})

test_that("mpd_validate_raw() errors when data is NULL", {
  expect_error(mpd_validate_raw(mpd = NULL, detail = FALSE))
})

# aux_maddison() ----------------------------------------------------------

test_that("aux_maddison() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

