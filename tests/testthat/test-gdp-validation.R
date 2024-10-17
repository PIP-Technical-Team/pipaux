
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "gdp"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("gdp_validate_output() works identifying duplicate error", {

  gdp <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdp[, `:=` (reporting_level = fifelse(reporting_level == "rural",
                                       "urban", reporting_level))]

  expect_error(gdp_validate_output(gdp))

})

test_that("gdp_validate_output() works identifying type/ formating error", {

  gdp <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdp[, `:=` (year = as.character(year),
              gdp = as.character(gdp))]

  expect_error(gdp_validate_output(gdp))

})

test_that("gdp_validate_output() works identifying invalid value", {

  gdp <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdp[, reporting_level := fifelse(reporting_level == "national",
                                  "national1", reporting_level)]

  expect_error(gdp_validate_output(gdp))

})
