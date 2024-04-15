
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "pce"
gls <- pipfun::pip_create_globals()

test_that("pce_validate_output() works identifying duplicate error", {

  pce <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pce[, `:=` (pce_data_level = fifelse(pce_data_level == "rural",
                                       "urban", pce_data_level))]

  expect_error(pce_validate_output(pce))

})

test_that("pce_validate_output() works identifying type/ formating error", {

  pce <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pce[, `:=` (year = as.character(year),
              pce = as.character(pce))]

  expect_error(pce_validate_output(pce))

})

test_that("pce_validate_output() works identifying invalid value", {

  pce <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pce[, pce_data_level := fifelse(pce_data_level == "national",
                                  "national1", pce_data_level)]

  expect_error(pce_validate_output(pce))

})
