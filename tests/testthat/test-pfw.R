# helper: valid raw pfw data ----------------------------------------------

make_pfw_raw <- function(...) {
  dt <- data.table::data.table(
    region             = c("Sub-Saharan Africa", "East Asia & Pacific"),
    code               = c("AAA", "BBB"),
    reg_pcn            = c("SSA", "EAP"),
    ctryname           = c("Country A", "Country B"),
    year               = c(2010, 2011),
    surveyid_year      = c(2010, 2011),
    timewp             = c(1.0, 2.0),
    fieldwork          = c(2010.0, 2011.0),
    survname           = c("Survey A", "Survey B"),
    link               = c("", ""),
    altname            = c("", ""),
    survey_time        = c("2010", "2011"),
    wbint_link         = c(1, 1),
    wbext_link         = c(1, 1),
    alt_link           = c(0, 0),
    pip_meta           = c(1, 1),
    surv_title         = c("Title A", "Title B"),
    surv_producer      = c("Producer A", "Producer B"),
    survey_coverage    = c("N", "U"),
    datatype           = c("C", "I"),
    use_imputed        = c(0, 1),
    use_microdata      = c(1, 0),
    use_bin            = c(0, 0),
    use_groupdata      = c(0, 1),
    rep_year           = c(2010, 2011),
    comparability      = c(1, 2),
    comp_note          = c("", ""),
    preferable         = c("", ""),
    display_cp         = c(1, 0),
    fieldwork_range    = c("", ""),
    ref_year           = c(2010.5, 2011.5),
    newref             = c("", ""),
    ref_year_des       = c(1.0, 2.0),
    wf_baseprice       = c("", ""),
    wf_baseprice_note  = c("", ""),
    wf_baseprice_des   = c(-9, -8),
    wf_spatial_des     = c(0, 1),
    wf_spatial_var     = c("", ""),
    cpi_replication    = c(-9, 1),
    cpi_domain         = c(1, 2),
    cpi_domain_var     = c("", ""),
    wf_currency_des    = c(0, 2),
    ppp_replication    = c(-9, 1),
    ppp_domain         = c(1, 2),
    ppp_domain_var     = c("", ""),
    wf_add_temp_des    = c(-9, 0),
    wf_add_temp_var    = c(0, 1),
    wf_add_spatial_des = c(-9, 0),
    wf_add_spatial_var = c(0, 1),
    tosplit            = c(NA_real_, 1),
    tosplit_var        = c("", ""),
    inpovcal           = c(1, 1),
    oth_welfare1_type  = c("", ""),
    oth_welfare1_var   = c("", ""),
    gdp_domain         = c(1, 2),
    pce_domain         = c(1, 2),
    pop_domain         = c(1, 2),
    pfw_id             = c("id1", "id2")
  )
  modifyList(dt, list(...))
}

# helper: valid output pfw data -------------------------------------------

make_pfw_output <- function(...) {
  dt <- data.table::data.table(
    region_code        = c("SSF", "EAS"),
    country_code       = c("AAA", "BBB"),
    reg_pcn            = c("SSA", "EAP"),
    ctryname           = c("Country A", "Country B"),
    year               = c(2010, 2011),
    surveyid_year      = c(2010, 2011),
    timewp             = c(1.0, 2.0),
    fieldwork          = c(2010.0, 2011.0),
    survey_acronym     = c("Survey A", "Survey B"),
    link               = c("", ""),
    altname            = c("", ""),
    survey_time        = c("2010", "2011"),
    wbint_link         = c(1, 1),
    wbext_link         = c(1, 1),
    alt_link           = c(0, 0),
    pip_meta           = c(1, 1),
    surv_title         = c("Title A", "Title B"),
    surv_producer      = c("Producer A", "Producer B"),
    survey_coverage    = c("national", "urban"),
    welfare_type       = c("consumption", "income"),
    use_imputed        = c(0, 1),
    use_microdata      = c(1, 0),
    use_bin            = c(0, 0),
    use_groupdata      = c(0, 1),
    reporting_year     = c(2010, 2011),
    survey_comparability = c(1, 2),
    comp_note          = c("", ""),
    preferable         = c("", ""),
    display_cp         = c(1, 0),
    fieldwork_range    = c("", ""),
    survey_year        = c(2010.5, 2011.5),
    newref             = c("", ""),
    ref_year_des       = c(1.0, 2.0),
    wf_baseprice       = c("", ""),
    wf_baseprice_note  = c("", ""),
    wf_baseprice_des   = c(-9, -8),
    wf_spatial_des     = c(0, 1),
    wf_spatial_var     = c("", ""),
    cpi_replication    = c(-9, 1),
    cpi_domain         = c(1, 2),
    cpi_domain_var     = c("", ""),
    wf_currency_des    = c(0, 2),
    ppp_replication    = c(-9, 1),
    ppp_domain         = c(1, 2),
    ppp_domain_var     = c("", ""),
    wf_add_temp_des    = c(-9, 0),
    wf_add_temp_var    = c(0, 1),
    wf_add_spatial_des = c(-9, 0),
    wf_add_spatial_var = c(0, 1),
    tosplit            = c(NA_real_, 1),
    tosplit_var        = c("", ""),
    inpovcal           = c(1, 1),
    oth_welfare1_type  = c("", ""),
    oth_welfare1_var   = c("", ""),
    gdp_domain         = c(1, 2),
    pce_domain         = c(1, 2),
    pop_domain         = c(1, 2),
    pfw_id             = c("id1", "id2")
  )
  modifyList(dt, list(...))
}

# pfw_validate_raw() ------------------------------------------------------


test_that("pfw_validate_raw() errors when region has invalid value", {
  bad <- make_pfw_raw(region = c("Sub-Saharan Africa", "INVALID"))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when reg_pcn has invalid value", {
  bad <- make_pfw_raw(reg_pcn = c("SSA", "INVALID"))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when survey_coverage has invalid value", {
  bad <- make_pfw_raw(survey_coverage = c("N", "INVALID"))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when datatype has invalid value", {
  bad <- make_pfw_raw(datatype = c("C", "INVALID"))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when use_imputed has invalid value", {
  bad <- make_pfw_raw(use_imputed = c(0, 99))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when cpi_domain has invalid value", {
  bad <- make_pfw_raw(cpi_domain = c(1, 99))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when ppp_domain has invalid value", {
  bad <- make_pfw_raw(ppp_domain = c(1, 99))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when wf_baseprice_des has invalid value", {
  bad <- make_pfw_raw(wf_baseprice_des = c(-9, 99))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when code is NA", {
  bad <- make_pfw_raw(code = c("AAA", NA_character_))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when year is NA", {
  bad <- make_pfw_raw(year = c(2010, NA_real_))
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors on duplicate key values", {
  bad <- make_pfw_raw(
    code     = c("AAA", "AAA"),
    year     = c(2010, 2010),
    survname = c("Survey A", "Survey A")
  )
  expect_error(pfw_validate_raw(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_raw() errors when data is NULL", {
  expect_error(pfw_validate_raw(pfw = NULL, detail = FALSE))
})

# pfw_validate_output() ---------------------------------------------------


test_that("pfw_validate_output() errors when region_code has invalid value", {
  bad <- make_pfw_output(region_code = c("SSF", "INVALID"))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when welfare_type has invalid value", {
  bad <- make_pfw_output(welfare_type = c("consumption", "invalid"))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when reg_pcn has invalid value", {
  bad <- make_pfw_output(reg_pcn = c("SSA", "INVALID"))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when use_bin has invalid value", {
  bad <- make_pfw_output(use_bin = c(0, 99))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when wf_baseprice_des has invalid value", {
  bad <- make_pfw_output(wf_baseprice_des = c(-9, 99))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when country_code is NA", {
  bad <- make_pfw_output(country_code = c("AAA", NA_character_))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when year is NA", {
  bad <- make_pfw_output(year = c(2010, NA_real_))
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors on duplicate key values", {
  bad <- make_pfw_output(
    country_code = c("AAA", "AAA"),
    year         = c(2010, 2010),
    welfare_type = c("consumption", "consumption")
  )
  expect_error(pfw_validate_output(pfw = bad, detail = FALSE))
})

test_that("pfw_validate_output() errors when data is NULL", {
  expect_error(pfw_validate_output(pfw = NULL, detail = FALSE))
})

# aux_pfw() / aux_pfw_update() --------------------------------------------

test_that("aux_pfw() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})



