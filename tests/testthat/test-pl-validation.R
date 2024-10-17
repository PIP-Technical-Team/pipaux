
## Initial parameters --------
branch  <- "DEV"
measure <- "pl"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("pl_validate_output() works identifying duplicate error", {

  pl <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pl[, `:=` (ppp_year = fifelse(ppp_year == 2011,
                                       2017, ppp_year))]

  expect_error(pl_validate_output(pl))

})

test_that("pl_validate_output() works identifying type/ formating error", {

  pl <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pl[, `:=` (poverty_line = as.character(poverty_line),
              ppp_year = as.character(ppp_year))]

  expect_error(pl_validate_output(pl))

})
