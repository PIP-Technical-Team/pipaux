# helper: valid missing_data output ---------------------------------------

make_missing_data_output <- function(...) {
  dt <- data.table::data.table(
    country_code   = c("AAA", "BBB"),
    year           = c(2010, 2011),
    region_code    = c("EAP", "ECA"),
    reporting_pop  = c(1000000, 2000000)
  )
  modifyList(dt, list(...))
}

# output structure --------------------------------------------------------

test_that("missing_data output has expected columns", {
  dt <- make_missing_data_output()
  expect_true(all(c("country_code", "year", "region_code", "reporting_pop") %in% names(dt)))
})

test_that("missing_data country_code is character", {
  dt <- make_missing_data_output()
  expect_type(dt$country_code, "character")
})

test_that("missing_data year is numeric", {
  dt <- make_missing_data_output()
  expect_type(dt$year, "double")
})

test_that("missing_data region_code is character", {
  dt <- make_missing_data_output()
  expect_type(dt$region_code, "character")
})

test_that("missing_data reporting_pop is numeric", {
  dt <- make_missing_data_output()
  expect_type(dt$reporting_pop, "double")
})

test_that("missing_data has no duplicate country_code/year combinations", {
  dt <- make_missing_data_output()
  expect_equal(nrow(dt), nrow(unique(dt[, .(country_code, year)])))
})

test_that("missing_data country_code has no NA values", {
  dt <- make_missing_data_output()
  expect_false(any(is.na(dt$country_code)))
})

test_that("missing_data year has no NA values", {
  dt <- make_missing_data_output()
  expect_false(any(is.na(dt$year)))
})

# aux_missing_data() ------------------------------------------------------

test_that("aux_missing_data() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

# helper: valid raw metadata data -----------------------------------------

make_metadata_raw <- function(...) {
  dt <- data.table::data.table(
    status                       = c("active", "active"),
    reg                          = c("EAP", "ECA"),
    id                           = c(1, 2),
    svy_id                       = c("AAA_2010_SURV", "BBB_2011_SURV"),
    link                         = c("link1", "link2"),
    title                        = c("Survey A", "Survey B"),
    data_access                  = c("public", "public"),
    year_start                   = c(2010, 2011),
    year_end                     = c(2010, 2011),
    authoring_entity_name        = c("NSO A", "NSO B"),
    authoring_entity_affiliation = c("Gov A", "Gov B"),
    contact_email                = c("a@a.com", "b@b.com"),
    contact_uri                  = c("", ""),
    abstract                     = c("", ""),
    collection_dates_cycle       = c("", ""),
    collection_dates_start       = c("", ""),
    collection_dates_end         = c("", ""),
    coverage                     = c("national", "urban"),
    sampling_procedure           = c("", ""),
    collection_mode              = c("", ""),
    coll_situation               = c("", ""),
    weight                       = c("", ""),
    cleaning_operations          = c("", ""),
    coverage_notes               = c("", "")
  )
  modifyList(dt, list(...))
}

# helper: valid output metadata data --------------------------------------

make_metadata_output <- function(...) {
  dt <- data.table::data.table(
    country_code      = c("AAA", "BBB"),
    country_name      = c("Country A", "Country B"),
    year              = c(2010, 2011),
    survey_year       = c(2010.5, 2011.5),
    surveyid_year     = c(2010, 2011),
    survey_title      = c("Survey A", "Survey B"),
    survey_conductor  = c("NSO A", "NSO B"),
    survey_coverage   = c("national", "urban"),
    welfare_type      = c("consumption", "income"),
    distribution_type = c("micro", "group"),
    metadata          = list(
      data.table::data.table(survey_acronym = "SURV_A"),
      data.table::data.table(survey_acronym = "SURV_B")
    )
  )
  modifyList(dt, list(...))
}

# metadata_validate_raw() -------------------------------------------------

test_that("metadata_validate_raw() passes with valid data", {
  expect_no_error(metadata_validate_raw(metadata = make_metadata_raw(), detail = FALSE))
})

test_that("metadata_validate_raw() errors when reg has invalid value", {
  bad <- make_metadata_raw(reg = c("EAP", "INVALID"))
  expect_error(metadata_validate_raw(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_raw() errors when svy_id is NA", {
  bad <- make_metadata_raw(svy_id = c("AAA_2010_SURV", NA_character_))
  expect_error(metadata_validate_raw(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_raw() errors on duplicate svy_id", {
  bad <- make_metadata_raw(svy_id = c("AAA_2010_SURV", "AAA_2010_SURV"))
  expect_error(metadata_validate_raw(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_raw() errors when id is not numeric", {
  bad <- make_metadata_raw(id = c("a", "b"))
  expect_error(metadata_validate_raw(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_raw() errors when year_start is not numeric", {
  bad <- make_metadata_raw(year_start = c("2010", "2011"))
  expect_error(metadata_validate_raw(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_raw() errors when status is not character", {
  bad <- make_metadata_raw(status = c(1, 2))
  expect_error(metadata_validate_raw(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_raw() errors when data is NULL", {
  expect_error(metadata_validate_raw(metadata = NULL, detail = FALSE))
})

# metadata_validate_output() ----------------------------------------------

test_that("metadata_validate_output() passes with valid data", {
  expect_no_error(metadata_validate_output(metadata = make_metadata_output(), detail = FALSE))
})

test_that("metadata_validate_output() errors when welfare_type has invalid value", {
  bad <- make_metadata_output(welfare_type = c("consumption", "invalid"))
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors when distribution_type has invalid value", {
  bad <- make_metadata_output(distribution_type = c("micro", "invalid"))
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors when country_code is NA", {
  bad <- make_metadata_output(country_code = c("AAA", NA_character_))
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors when year is NA", {
  bad <- make_metadata_output(year = c(2010, NA_real_))
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors when welfare_type is NA", {
  bad <- make_metadata_output(welfare_type = c("consumption", NA_character_))
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors on duplicate key values", {
  bad <- make_metadata_output(
    country_code = c("AAA", "AAA"),
    year         = c(2010, 2010),
    welfare_type = c("consumption", "consumption")
  )
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors when year is not numeric", {
  bad <- make_metadata_output(year = c("2010", "2011"))
  expect_error(metadata_validate_output(metadata = bad, detail = FALSE))
})

test_that("metadata_validate_output() errors when data is NULL", {
  expect_error(metadata_validate_output(metadata = NULL, detail = FALSE))
})

# aux_metadata() ----------------------------------------------------------

test_that("aux_metadata() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})