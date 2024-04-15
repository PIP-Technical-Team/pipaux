
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "npl"
gls <- pipfun::pip_create_globals()

test_that("npl_validate_raw() works identifying duplicate error", {

  npl <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "dta"
  ) |> setDT()

  npl[, `:=` (year = fifelse((year == 2007 & countrycode == "AFG"),
                                 2011, year))]

  expect_error(npl_validate_raw(npl))

})

test_that("npl_validate_raw() works identifying type/ formating error", {

  npl <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "dta"
  ) |> setDT()

  npl[, `:=` (year = as.character(year),
              comparability = as.character(comparability))]

  expect_error(npl_validate_raw(npl))

})


test_that("npl_validate_output() works identifying duplicate error", {

  npl <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  npl[, `:=` (reporting_year = fifelse((reporting_year == 2007 & country_code == "AFG"),
                             2011, reporting_year))]

  expect_error(npl_validate_output(npl))

})

test_that("npl_validate_output() works identifying type/ formating error", {

  npl <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  npl[, `:=` (reporting_year = as.character(reporting_year),
              comparability = as.character(comparability),
              nat_headcount = as.character(nat_headcount))]

  expect_error(npl_validate_output(npl))

})

