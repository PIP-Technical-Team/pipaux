
# helper: valid output pce data -------------------------------------------

make_pce_output <- function(...) {
  dt <- data.table::data.table(
    country_code    = c("AAA", "BBB", "CCC"),
    year            = c(2010, 2011, 2012),
    reporting_level = c("national", "urban", "rural"),
    pce             = c(1000.5, 2000.5, 3000.5)
  )
  modifyList(dt, list(...))
}

# pce_validate_output() ---------------------------------------------------

test_that("pce_validate_output() errors when reporting_level has invalid value", {
  bad <- make_pce_output(reporting_level = c("national", "urban", "invalid"))
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors when country_code is NA", {
  bad <- make_pce_output(country_code = c("AAA", NA_character_, "CCC"))
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors when year is NA", {
  bad <- make_pce_output(year = c(2010, NA_real_, 2012))
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors when reporting_level is NA", {
  bad <- make_pce_output(reporting_level = c("national", NA_character_, "rural"))
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors when pce is not numeric", {
  bad <- make_pce_output(pce = c("a", "b", "c"))
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors when country_code is not character", {
  bad <- make_pce_output(country_code = c(1, 2, 3))
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors on duplicate key values", {
  bad <- make_pce_output(
    country_code    = c("AAA", "AAA", "CCC"),
    year            = c(2010, 2010, 2012),
    reporting_level = c("national", "national", "rural")
  )
  expect_error(pce_validate_output(pce = bad, detail = FALSE))
})

test_that("pce_validate_output() errors when data is NULL", {
  expect_error(pce_validate_output(pce = NULL, detail = FALSE))
})

# aux_pce() / aux_pce_update() --------------------------------------------

test_that("aux_pce() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

