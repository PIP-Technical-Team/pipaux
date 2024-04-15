
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "gdm"
gls <- pipfun::pip_create_globals()

test_that("gdm_validate_raw() works identifying duplicate error", {

  gdm <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  gdm[, `:=` (Coverage = fifelse(Coverage == "Urban",
                                       "Rural", Coverage))]

  expect_error(gdm_validate_raw(gdm))

})

test_that("gdm_validate_raw() works identifying type/ formating error", {

  gdm <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
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
    branch = branch
  )

  gdm[, DataType := fifelse(DataType == "x", "i", DataType)]

  expect_error(gdm_validate_raw(gdm))

})

test_that("gdm_validate_output() works identifying duplicate error", {

  gdm <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdm[, `:=` (pop_data_level = fifelse(pop_data_level == "rural",
                                       "urban", pop_data_level))]

  expect_error(gdm_validate_output(gdm))

})

test_that("gdm_validate_output() works identifying type/ formating error", {

  gdm <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdm[, `:=` (surveyid_year = as.character(surveyid_year),
              survey_year = as.character(survey_year),
              survey_mean_lcu = as.character(survey_mean_lcu))]

  expect_error(gdm_validate_output(gdm))

})

test_that("gdm_validate_output() works identifying invalid value", {

  gdm <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdm[, pop_data_level := fifelse(pop_data_level == "national",
                                  "national1", pop_data_level)]

  expect_error(gdm_validate_output(gdm))

})
