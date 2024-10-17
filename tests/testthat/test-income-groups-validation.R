
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "income_groups"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("gdp_validate_output() works identifying type/ formating error", {

  incgroups <- load_aux(
    maindir = temp_fld,  #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  incgroups[, `:=` (year_data = as.character(year_data),
              year = as.character(year))]

  expect_error(incgroup_validate_output(incgroups))

})

test_that("incgroup_validate_output() works identifying invalid value", {

  incgroups <- load_aux(
    maindir = temp_fld,  #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  incgroups[, income_group_code := fifelse(income_group_code == "HIC",
                                  "hic", income_group_code)]

  expect_error(incgroup_validate_output(incgroups))

})
