
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "gdp"
gls <- pipfun::pip_create_globals()

test_that("gdp_validate_output() works identifying duplicate error", {

  gdp <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdp[, `:=` (gdp_data_level = fifelse(gdp_data_level == "rural",
                                       "urban", gdp_data_level))]

  expect_error(gdp_validate_output(gdp), "Duplicate error")

})

test_that("gdp_validate_output() works identifying type/ formating error", {

  gdp <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdp[, `:=` (year = as.character(year),
              gdp = as.character(gdp))]

  expect_error(gdp_validate_output(gdp), "Type/ format error")

})

test_that("gdp_validate_output() works identifying invalid value", {

  gdp <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  gdp[, gdp_data_level := fifelse(gdp_data_level == "national",
                                  "national1", gdp_data_level)]

  expect_error(gdp_validate_output(gdp), "Invalid value in `gdp_data_level`")

})
