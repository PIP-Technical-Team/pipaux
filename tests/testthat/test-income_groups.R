

# helper: valid output income groups data ---------------------------------

make_incgroup_output <- function(...) {
  dt <- data.table::data.table(
    country_code       = c("AAA", "BBB"),
    year_data          = c(2010, 2011),
    income_group       = c("High income", "Low income"),
    income_group_code  = c("HIC", "LIC"),
    incgroup           = c("High income", "Low income"),
    fcv                = c("", ""),
    ssa_subregion_code = c("", "")
  )
  modifyList(dt, list(...))
}

# incgroup_validate_output() ----------------------------------------------


test_that("incgroup_validate_output() errors when income_group has invalid value", {
  bad <- make_incgroup_output(income_group = c("High income", "Invalid"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when income_group_code has invalid value", {
  bad <- make_incgroup_output(income_group_code = c("HIC", "INVALID"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when incgroup has invalid value", {
  bad <- make_incgroup_output(incgroup = c("High income", "Invalid"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when country_code is not character", {
  bad <- make_incgroup_output(country_code = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when year_data is not numeric", {
  bad <- make_incgroup_output(year_data = c("2010", "2011"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when income_group is not character", {
  bad <- make_incgroup_output(income_group = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when income_group_code is not character", {
  bad <- make_incgroup_output(income_group_code = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when fcv is not character", {
  bad <- make_incgroup_output(fcv = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when ssa_subregion_code is not character", {
  bad <- make_incgroup_output(ssa_subregion_code = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when country_code is NA", {
  bad <- make_incgroup_output(country_code = c("AAA", NA_character_))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when year_data is NA", {
  bad <- make_incgroup_output(year_data = c(2010, NA_real_))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors on duplicate key values", {
  bad <- make_incgroup_output(
    country_code = c("AAA", "AAA"),
    year_data    = c(2010, 2010)
  )
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when data is NULL", {
  expect_error(incgroup_validate_output(incgroup = NULL, detail = FALSE))
})

# aux_income_groups() -----------------------------------------------------

test_that("aux_income_groups() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

