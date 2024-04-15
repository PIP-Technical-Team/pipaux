
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure = "country_list"
gls <- pipfun::pip_create_globals()

test_that("cl_validate_raw() works identifying duplicate error", {

  cl <- pip_country_list_update(class_branch = "master")

  cl[, `:=` (country_code = fifelse(country_code == "AGO",
                                    "ALB", country_code))]

  expect_error(cl_validate_raw(cl))

})

test_that("cl_validate_raw() works identifying invalid value", {

  cl <- pip_country_list_update(class_branch = "master")

  cl[, `:=` (africa_split_code = fifelse(africa_split_code == "AFE",
                                          "SSA", africa_split_code),
             pcn_region_code = fifelse(pcn_region_code == "SSA",
                                         "SAR", pcn_region_code),
             region_code = fifelse(region_code == "SSA",
                                       "SAR", region_code))]

  expect_error(cl_validate_raw(cl))

})

test_that("cl_validate_raw() works identifying duplicate error", {

  cl <- load_aux(maindir = gls$PIP_DATA_DIR,
                 measure = measure,
                 branch  = branch)

  cl[, `:=` (country_code = fifelse(country_code == "AGO",
                                    "ALB", country_code))]

  expect_error(cl_validate_raw(cl))

})

test_that("cl_validate_raw() works identifying invalid value", {

  cl <- load_aux(maindir = gls$PIP_DATA_DIR,
                 measure = measure,
                 branch  = branch)

  cl[, `:=` (africa_split_code = fifelse(africa_split_code == "AFE",
                                         "SSA", africa_split_code),
             pcn_region_code = fifelse(pcn_region_code == "SSA",
                                       "SAR", pcn_region_code),
             region_code = fifelse(region_code == "SSA",
                                   "SAR", region_code))]

  expect_error(cl_validate_raw(cl))

})
