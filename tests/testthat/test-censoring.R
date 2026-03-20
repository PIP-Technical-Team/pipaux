# helper: valid censoring countries data ----------------------------------

make_censoring_countries <- function(...) {
  dt <- data.table::data.table(
    country_code    = c("AAA", "BBB"),
    reporting_year  = c(2010, 2011),
    survey_acronym  = c("SURV_A", "SURV_B"),
    welfare_type    = c("consumption", "income"),
    reporting_level = c("national", "urban")
  )
  modifyList(dt, list(...))
}

# helper: valid censoring regions data ------------------------------------

make_censoring_regions <- function(...) {
  dt <- data.table::data.table(
    region_code    = c("EAP", "ECA"),
    reporting_year = c(2010, 2011)
  )
  modifyList(dt, list(...))
}

# id column construction --------------------------------------------------

test_that("censoring countries id is constructed correctly", {
  dt <- make_censoring_countries()
  dt[, id := paste(country_code, reporting_year,
                   survey_acronym, welfare_type,
                   reporting_level, sep = "_")]
  expect_equal(dt$id[1], "AAA_2010_SURV_A_consumption_national")
  expect_equal(dt$id[2], "BBB_2011_SURV_B_income_urban")
})

test_that("censoring regions id is constructed correctly", {
  dt <- make_censoring_regions()
  dt[, id := paste(region_code, reporting_year, sep = "_")]
  expect_equal(dt$id[1], "EAP_2010")
  expect_equal(dt$id[2], "ECA_2011")
})

test_that("censoring countries id has no NA values with valid data", {
  dt <- make_censoring_countries()
  dt[, id := paste(country_code, reporting_year,
                   survey_acronym, welfare_type,
                   reporting_level, sep = "_")]
  expect_false(any(is.na(dt$id)))
})

test_that("censoring regions id has no NA values with valid data", {
  dt <- make_censoring_regions()
  dt[, id := paste(region_code, reporting_year, sep = "_")]
  expect_false(any(is.na(dt$id)))
})

test_that("censoring countries id is unique with valid data", {
  dt <- make_censoring_countries()
  dt[, id := paste(country_code, reporting_year,
                   survey_acronym, welfare_type,
                   reporting_level, sep = "_")]
  expect_equal(length(unique(dt$id)), nrow(dt))
})

test_that("censoring regions id is unique with valid data", {
  dt <- make_censoring_regions()
  dt[, id := paste(region_code, reporting_year, sep = "_")]
  expect_equal(length(unique(dt$id)), nrow(dt))
})

# output structure --------------------------------------------------------

test_that("censoring list has countries and regions elements", {
  dl <- list(
    countries = make_censoring_countries(),
    regions   = make_censoring_regions()
  )
  expect_named(dl, c("countries", "regions"))
})

test_that("censoring countries has expected columns", {
  dt <- make_censoring_countries()
  expect_true(all(c("country_code", "reporting_year",
                    "survey_acronym", "welfare_type",
                    "reporting_level") %in% names(dt)))
})

test_that("censoring regions has expected columns", {
  dt <- make_censoring_regions()
  expect_true(all(c("region_code", "reporting_year") %in% names(dt)))
})

test_that("censoring countries welfare_type is character", {
  dt <- make_censoring_countries()
  expect_type(dt$welfare_type, "character")
})

test_that("censoring countries reporting_level is character", {
  dt <- make_censoring_countries()
  expect_type(dt$reporting_level, "character")
})

test_that("censoring countries reporting_year is numeric", {
  dt <- make_censoring_countries()
  expect_type(dt$reporting_year, "double")
})

test_that("censoring regions reporting_year is numeric", {
  dt <- make_censoring_regions()
  expect_type(dt$reporting_year, "double")
})

# aux_censoring() ---------------------------------------------------------

test_that("aux_censoring() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})