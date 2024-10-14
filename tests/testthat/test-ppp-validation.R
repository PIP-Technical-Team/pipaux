
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "ppp"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("ppp_validate_raw() works identifying duplicate error", {

  ppp <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  ppp[, `:=` (CoverageType = fifelse(CoverageType == "Urban",
                                 "Rural1", CoverageType))]

  expect_error(ppp_validate_raw(ppp))

})

test_that("ppp_validate_raw() works identifying type/ formating error", {

  ppp <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  ppp[, `:=` (ppp_2017_v1_v1 = as.character(ppp_2017_v1_v1),
              ppp_2017_v1_v2 = as.character(ppp_2017_v1_v2),
              ppp_2011_v2_v1 = as.character(ppp_2011_v2_v1),
              ppp_2011_v2_v2 = as.character(ppp_2011_v2_v2))]

  expect_error(ppp_validate_raw(ppp))

})

test_that("ppp_validate_raw() works identifying invalid value", {

  ppp <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  ppp[, ppp_domain := fifelse(ppp_domain == 1, 3, ppp_domain)]

  expect_error(ppp_validate_raw(ppp))

})

test_that("ppp_validate_output() works identifying duplicate error", {

  ppp <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch,
    ppp_defaults = FALSE
  )

  ppp[, `:=` (reporting_level = fifelse(reporting_level == "rural",
                                       "urban", reporting_level))]

  expect_error(ppp_validate_output(ppp))

})

test_that("ppp_validate_output() works identifying type/ formating error", {

  ppp <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch,
    ppp_defaults = FALSE
  )

  ppp[, `:=` (ppp_year = as.character(ppp_year),
              ppp = as.character(ppp))]

  expect_error(ppp_validate_output(ppp))

})

test_that("ppp_validate_output() works identifying invalid value", {

  ppp <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch,
    ppp_defaults = FALSE
  )

  ppp[, reporting_level := fifelse(reporting_level == "national",
                                  "national1", reporting_level)]

  expect_error(ppp_validate_output(ppp))

})
