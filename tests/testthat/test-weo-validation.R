
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "weo"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("weo_validate_raw() works identifying duplicate error", {

  weo <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  weo[, `:=` (`WEO Subject Code` = fifelse(`WEO Subject Code` == "NGDP",
                                 "NGDPD", `WEO Subject Code`))]

  expect_error(weo_validate_raw(weo))

})


test_that("weo_validate_output() works identifying duplicate error", {

  weo <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  weo[, `:=` (year = fifelse(year == 1986 & country_code == "ABW",
                                       1987, year))]

  expect_error(weo_validate_output(weo))

})

test_that("weo_validate_output() works identifying type/ formating error", {

  weo <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  weo[, `:=` (year = as.character(year),
              weo_gdp = as.character(weo_gdp))]

  expect_error(weo_validate_output(weo))

})
