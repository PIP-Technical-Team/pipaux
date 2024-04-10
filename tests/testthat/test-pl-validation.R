
## Initial parameters --------
branch  <- "DEV"
measure <- "pl"
gls <- pipfun::pip_create_globals()

test_that("pl_validate_output() works identifying duplicate error", {

  pl <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pl[, `:=` (ppp_year = fifelse(ppp_year == 2011,
                                       2017, ppp_year))]

  expect_error(pl_validate_output(pl), "Duplicate error")

})

test_that("pl_validate_output() works identifying type/ formating error", {

  pl <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pl[, `:=` (poverty_line = as.character(poverty_line),
              ppp_year = as.character(ppp_year))]

  expect_error(pl_validate_output(pl), "Type/ format error")

})
