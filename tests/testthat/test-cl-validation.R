
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure = "country_list"

test_that("cl_validate_raw() works identifying duplicate error", {

  cl <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  cl[, `:=` (country_code = fifelse(country_code == "ABW",
                                    "ALB", country_code))]

  expect_error(cl_validate_raw(cl), "Duplicate error")

})

test_that("cl_validate_raw() works identifying invalid value", {

  cl <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  cl[, `:=` (africa_split_code = fifelse(africa_split_code == "AFE",
                                          "SSA", africa_split_code),
             pcn_region_code = fifelse(pcn_region_code == "SSA",
                                         "SAR", pcn_region_code),
             region_code = fifelse(region_code == "SSA",
                                       "SAR", region_code))]

  expect_error(cl_validate_raw(cl), "Invalid values")

})
