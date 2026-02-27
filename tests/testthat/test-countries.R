# helper: valid output countries data -------------------------------------

make_countries_output <- function(...) {
  dt <- data.table::data.table(
    country_code      = c("AAA", "BBB"),
    country_name      = c("Country A", "Country B"),
    africa_split      = c("Eastern and Southern Africa", NA),
    africa_split_code = c("AFE", NA),
    region            = c("East Asia & Pacific", "Europe & Central Asia"),
    region_code       = c("EAP", "ECA"),
    world             = c("World", "World"),
    world_code        = c("WLD", "WLD")
  )
  modifyList(dt, list(...))
}

# countries_validate_output() ---------------------------------------------

test_that("countries_validate_output() passes with valid data", {
  expect_no_error(countries_validate_output(countries = make_countries_output(), detail = FALSE))
})

test_that("countries_validate_output() errors when country_code is not character", {
  bad <- make_countries_output(country_code = c(1, 2))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when country_name is not character", {
  bad <- make_countries_output(country_name = c(1, 2))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when region_code has invalid value", {
  bad <- make_countries_output(region_code = c("EAP", "INVALID"))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when africa_split has invalid value", {
  bad <- make_countries_output(africa_split = c("Eastern and Southern Africa", "Invalid"))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when africa_split_code has invalid value", {
  bad <- make_countries_output(africa_split_code = c("AFE", "INVALID"))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when world has invalid value", {
  bad <- make_countries_output(world = c("World", "Invalid"))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when world_code has invalid value", {
  bad <- make_countries_output(world_code = c("WLD", "INVALID"))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when country_code is NA", {
  bad <- make_countries_output(country_code = c("AAA", NA_character_))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors on duplicate key values", {
  bad <- make_countries_output(country_code = c("AAA", "AAA"))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when region is not character", {
  bad <- make_countries_output(region = c(1, 2))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when region_code is not character", {
  bad <- make_countries_output(region_code = c(1, 2))
  expect_error(countries_validate_output(countries = bad, detail = FALSE))
})

test_that("countries_validate_output() errors when data is NULL", {
  expect_error(countries_validate_output(countries = NULL, detail = FALSE))
})

# aux_countries() ---------------------------------------------------------
