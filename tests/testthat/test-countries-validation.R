
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "countries"
gls     <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("countries_validate_output() works identifying duplicate error", {

  countries <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  countries[, `:=` (country_code = fifelse(country_code == "AGO",
                                    "ALB", country_code))]

  expect_error(countries_validate_output(countries))

})

test_that("countries_validate_output() works identifying invalid value", {

  countries <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  countries[, `:=` (africa_split_code = fifelse(africa_split_code == "AFE",
                                         "SSA", africa_split_code),
             # pcn_region_code = fifelse(pcn_region_code == "SSA",
             #                           "SAR", pcn_region_code),
             region_code = fifelse(region_code == "SSA",
                                   "SAR", region_code))]

  expect_error(countries_validate_output(countries))

})
