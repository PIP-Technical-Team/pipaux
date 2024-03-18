
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure = "cpi"

test_that("cpi_validate_raw() works identifying duplicate error", {

  cpi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  cpi[, `:=` (cpi_data_level = fifelse(cpi_data_level == 0,
                                    1, cpi_data_level))]

  expect_error(cpi_validate_raw(cpi), "Duplicate error")

})


test_that("cpi_validate_raw() works identifying type/ formating error", {

  cpi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  cpi[, `:=` (year = as.character(year),
              ref_year = as.character(year),
              cpi_domain_value = as.character(cpi_domain_value),
              cpi2011 = as.character(cpi2011),
              cpi2017 = as.character(cpi2017))]

  expect_error(cpi_validate_raw(cpi), "Type/ format error")

})

test_that("cpi_validate_raw() works identifying invalid value", {

  cpi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  cpi[, cpi_domain := fifelse(cpi_domain == "National", "National1", cpi_domain)]

  expect_error(cpi_validate_raw(cpi), "Invalid value in `cpi_domain`")

})

test_that("cpi_validate_output() works identifying duplicate error", {

  cpi <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  cpi[, `:=` (cpi_data_level = fifelse(cpi_data_level == "rural",
                                       "urban", cpi_data_level))]

  expect_error(cpi_validate_output(cpi), "Duplicate error")

})

test_that("cpi_validate_output() works identifying type/ formating error", {

  cpi <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  cpi[, `:=` (cpi_year = as.character(cpi_year),
              survey_year = as.character(survey_year),
              cpi_domain_value = as.character(cpi_domain_value),
              cpi2011 = as.character(cpi2011),
              cpi2017 = as.character(cpi2017))]

  expect_error(cpi_validate_output(cpi), "Type/ format error")

})


test_that("cpi_validate_output() works identifying invalid value", {

  cpi <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  cpi[, cpi_domain := fifelse(cpi_domain == "National", "National1", cpi_domain)]

  expect_error(cpi_validate_output(cpi), "Invalid value in `cpi_domain`")

})
