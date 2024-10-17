
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "gdm"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("gdm_validate_raw() works identifying duplicate error", {

  gdm <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  gdm[, `:=` (Coverage = fifelse(Coverage == "Urban",
                                       "Rural", Coverage))]

  expect_error(gdm_validate_raw(gdm))

})

test_that("gdm_validate_raw() works identifying type/ formating error", {

  gdm <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  gdm[, `:=` (SurveyTime = as.character(SurveyTime),
              CPI_Time = as.character(CPI_Time),
              SurveyMean_LCU = as.character(SurveyMean_LCU),
              currency = as.character(currency),
              SurveyMean_PPP = as.character(SurveyMean_PPP))]

  expect_error(gdm_validate_raw(gdm))

})

test_that("gdm_validate_raw() works identifying invalid value", {

  gdm <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  gdm[, DataType := fifelse(DataType == "x", "i", DataType)]

  expect_error(gdm_validate_raw(gdm))

})

test_that("gdm_validate_output() works identifying duplicate error", {

  gdm <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdm[, `:=` (reporting_level = fifelse(reporting_level == "rural",
                                       "urban", reporting_level))]

  expect_error(gdm_validate_output(gdm))

})

test_that("gdm_validate_output() works identifying type/ formating error", {

  gdm <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdm[, `:=` (year = as.character(year),
              survey_year = as.character(survey_year),
              survey_mean_lcu = as.character(survey_mean_lcu))]

  expect_error(gdm_validate_output(gdm))

})

test_that("gdm_validate_output() works identifying invalid value", {

  gdm <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdm[, reporting_level := fifelse(reporting_level == "national",
                                  "national1", reporting_level)]

  expect_error(gdm_validate_output(gdm))

})
