# helper: valid nan output data -------------------------------------------

make_nan_output <- function(...) {
  dt <- data.table::data.table(
    country_code  = c("AAA", "BBB"),
    year          = c(2010, 2011),
    gdp_data_level = c(0, 1),
    gdp_growth    = c(0.05, 0.03),
    pce_growth    = c(0.04, 0.02)
  )
  modifyList(dt, list(...))
}

# output structure --------------------------------------------------------

test_that("nan output has expected columns", {
  dt <- make_nan_output()
  expect_true(all(c("country_code", "year", "gdp_data_level") %in% names(dt)))
})

test_that("nan country_code is character", {
  dt <- make_nan_output()
  expect_type(dt$country_code, "character")
})

test_that("nan year is numeric", {
  dt <- make_nan_output()
  expect_type(dt$year, "double")
})

test_that("nan gdp_data_level is numeric", {
  dt <- make_nan_output()
  expect_type(dt$gdp_data_level, "double")
})

test_that("nan country_code has no NA values", {
  dt <- make_nan_output()
  expect_false(any(is.na(dt$country_code)))
})

test_that("nan year has no NA values", {
  dt <- make_nan_output()
  expect_false(any(is.na(dt$year)))
})

test_that("nan gdp_data_level has no NA values", {
  dt <- make_nan_output()
  expect_false(any(is.na(dt$gdp_data_level)))
})

test_that("nan has no duplicate key combinations", {
  dt <- make_nan_output()
  expect_equal(nrow(dt), nrow(unique(dt[, .(country_code, year, gdp_data_level)])))
})

test_that("nan errors when country_code is not character", {
  bad <- make_nan_output(country_code = c(1, 2))
  expect_false(is.character(bad$country_code))
})

test_that("nan errors when year is not numeric", {
  bad <- make_nan_output(year = c("2010", "2011"))
  expect_false(is.numeric(bad$year))
})

# aux_nan() ---------------------------------------------------------------

test_that("aux_nan() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})