
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "pfw"
gls <- pipfun::pip_create_globals()

test_that("pfw_validate_raw() works identifying duplicate error", {

  pfw <- pipfun::load_from_gh(measure = measure,
                              owner = owner,
                              branch = branch,
                              ext = "dta")

  pfw[, `:=` (year = fifelse((year == 1981 & code == "CHN"),
                                 1984, year))]

  expect_error(pfw_validate_raw(pfw), "Duplicate error")

})

test_that("pfw_validate_raw() works identifying type/ formating error", {

  pfw <- pipfun::load_from_gh(measure = measure,
                              owner = owner,
                              branch = branch,
                              ext = "dta")

  pfw[, `:=` (year = as.character(year),
              gdp_domain = as.character(gdp_domain),
              pce_domain = as.character(pce_domain),
              pop_domain = as.character(pop_domain))]

  expect_error(pfw_validate_raw(pfw), "Type/ format error")

})

test_that("pfw_validate_raw() works identifying invalid value", {

  pfw <- pipfun::load_from_gh(measure = measure,
                              owner = owner,
                              branch = branch,
                              ext = "dta")

  pfw[, datatype := fifelse(datatype == "c", "x", datatype)]

  expect_error(pfw_validate_raw(pfw), "Invalid value in `datatype`")

})

test_that("pfw_validate_raw() works identifying invalid value", {

  pfw <- pipfun::load_from_gh(measure = measure,
                              owner = owner,
                              branch = branch,
                              ext = "dta")

  pfw[, pce_domain := fifelse(pce_domain == 1, 3, pce_domain)]

  expect_error(pfw_validate_raw(pfw), "Invalid value in `pce_domain`")

})

test_that("pfw_validate_output() works identifying duplicate error", {

  pfw <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pfw[, `:=` (welfare_type = fifelse(welfare_type == "consumption",
                                       "income", welfare_type))]

  expect_error(pfw_validate_output(pfw), "Duplicate error")

})

test_that("pfw_validate_output() works identifying type/ formating error", {

  pfw <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pfw[, `:=` (year = as.character(year),
              gdp_domain = as.character(gdp_domain),
              pce_domain = as.character(pce_domain),
              pop_domain = as.character(pop_domain))]

  expect_error(pfw_validate_output(pfw), "Type/ format error")

})

test_that("pfw_validate_output() works identifying invalid value", {

  pfw <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pfw[, pce_domain := fifelse(pce_domain == 1, 3, pce_domain)]

  expect_error(pfw_validate_output(pfw), "Invalid value in `pce_domain`")

})
