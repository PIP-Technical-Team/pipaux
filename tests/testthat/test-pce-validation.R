
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "pce"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("pce_validate_output() works identifying duplicate error", {

  pce <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pce[, `:=` (reporting_level = fifelse(reporting_level == "rural",
                                       "urban", reporting_level))]

  expect_error(pce_validate_output(pce))

})

test_that("pce_validate_output() works identifying type/ formating error", {

  pce <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pce[, `:=` (year = as.character(year),
              pce = as.character(pce))]

  expect_error(pce_validate_output(pce))

})

test_that("pce_validate_output() works identifying invalid value", {

  pce <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pce[, reporting_level := fifelse(reporting_level == "national",
                                  "national1", reporting_level)]

  expect_error(pce_validate_output(pce))

})
