# helper: valid raw gdm data ----------------------------------------------

make_gdm_raw <- function(...) {
  dt <- data.table::data.table(
    Region               = c("EAP", "ECA"),
    countryName          = c("Country A", "Country B"),
    CountryCode          = c("AAA", "BBB"),
    Coverage             = c("National", "Urban"),
    SurveyTime           = c(2010.5, 2011.5),
    CPI_Time             = c(2010.5, 2011.5),
    DataType             = c("x", "Y"),
    SurveyMean_LCU       = c(1000.0, 2000.0),
    currency             = c(1.0, 2.0),
    source               = c("source1", "source2"),
    SurveyID             = c("AAA_2010", "BBB_2011"),
    SurveyMean_PPP       = c(500.0, 1000.0),
    DistributionFileName = c("AAA_2010.T01", "BBB_2011.T02"),
    Comment              = c(FALSE, FALSE)
  )
  modifyList(dt, list(...))
}

# helper: valid output gdm data -------------------------------------------

make_gdm_output <- function(...) {
  dt <- data.table::data.table(
    survey_id        = c("AAA_2010_SURV", "BBB_2011_SURV"),
    country_code     = c("AAA", "BBB"),
    year             = c(2010L, 2011L),
    survey_year      = c(2010.5, 2011.5),
    welfare_type     = c("consumption", "income"),
    survey_mean_lcu  = c(1000.0, 2000.0),
    distribution_type = c("group", "aggregate"),
    gd_type          = c("T01", "T02"),
    reporting_level  = c("national", "urban"),
    pcn_source_file  = c("AAA_2010.T01", "BBB_2011.T02"),
    pcn_survey_id    = c("AAA_2010", "BBB_2011")
  )
  modifyList(dt, list(...))
}

# gdm_validate_raw() ------------------------------------------------------

test_that("gdm_validate_raw() passes with valid data", {
  expect_no_error(gdm_validate_raw(gdm = make_gdm_raw(), detail = FALSE))
})

test_that("gdm_validate_raw() errors when Region has invalid value", {
  bad <- make_gdm_raw(Region = c("EAP", "INVALID"))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when Coverage has invalid value", {
  bad <- make_gdm_raw(Coverage = c("National", "Invalid"))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when DataType has invalid value", {
  bad <- make_gdm_raw(DataType = c("x", "INVALID"))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when CountryCode is NA", {
  bad <- make_gdm_raw(CountryCode = c("AAA", NA_character_))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when Coverage is NA", {
  bad <- make_gdm_raw(Coverage = c("National", NA_character_))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when SurveyTime is NA", {
  bad <- make_gdm_raw(SurveyTime = c(2010.5, NA_real_))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when SurveyTime is not numeric", {
  bad <- make_gdm_raw(SurveyTime = c("2010", "2011"))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when SurveyMean_LCU is not numeric", {
  bad <- make_gdm_raw(SurveyMean_LCU = c("a", "b"))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors on duplicate key values", {
  bad <- make_gdm_raw(
    CountryCode = c("AAA", "AAA"),
    Coverage    = c("National", "National"),
    SurveyTime  = c(2010.5, 2010.5),
    DataType    = c("x", "x")
  )
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when Comment is not logical", {
  bad <- make_gdm_raw(Comment = c(1, 0))
  expect_error(gdm_validate_raw(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_raw() errors when data is NULL", {
  expect_error(gdm_validate_raw(gdm = NULL, detail = FALSE))
})

# gdm_validate_output() ---------------------------------------------------

test_that("gdm_validate_output() passes with valid data", {
  expect_no_error(gdm_validate_output(gdm = make_gdm_output(), detail = FALSE))
})

test_that("gdm_validate_output() errors when welfare_type has invalid value", {
  bad <- make_gdm_output(welfare_type = c("consumption", "invalid"))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when distribution_type has invalid value", {
  bad <- make_gdm_output(distribution_type = c("group", "invalid"))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when reporting_level has invalid value", {
  bad <- make_gdm_output(reporting_level = c("national", "invalid"))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when country_code is NA", {
  bad <- make_gdm_output(country_code = c("AAA", NA_character_))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when year is NA", {
  bad <- make_gdm_output(year = c(2010L, NA_integer_))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when reporting_level is NA", {
  bad <- make_gdm_output(reporting_level = c("national", NA_character_))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when year is not integer", {
  bad <- make_gdm_output(year = c(2010.5, 2011.5))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when survey_mean_lcu is not numeric", {
  bad <- make_gdm_output(survey_mean_lcu = c("a", "b"))
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors on duplicate key values", {
  bad <- make_gdm_output(
    country_code    = c("AAA", "AAA"),
    year            = c(2010L, 2010L),
    reporting_level = c("national", "national")
  )
  expect_error(gdm_validate_output(gdm = bad, detail = FALSE))
})

test_that("gdm_validate_output() errors when data is NULL", {
  expect_error(gdm_validate_output(gdm = NULL, detail = FALSE))
})

# aux_gdm() / aux_gdm_update() --------------------------------------------

test_that("aux_gdm() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

