
## Initial parameters --------

branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "metadata"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("metadata_validate_raw() works identifying duplicate error", {

  metadata <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  metadata[, `:=` (svy_id = fifelse(svy_id == "CNH_2005_URHS_v01_M",
                                 "CNH_2008_URHS_v01_M", svy_id))]

  expect_error(metadata_validate_raw(metadata))

})

test_that("metadata_validate_raw() works identifying type/ formating error", {

  metadata <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  metadata[, `:=` (year_start = as.character(year_start),
              year_end = as.character(year_end))]

  expect_error(metadata_validate_raw(metadata))

})

test_that("metadata_validate_raw() works identifying invalid value", {

  metadata <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  metadata[, reg := fifelse(reg == "SSA", "ssa", reg)]

  expect_error(metadata_validate_raw(metadata))

})

test_that("metadata_validate_output() works identifying duplicate error", {

  metadata <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  metadata[, `:=` (welfare_type = fifelse(welfare_type == "consumption",
                                       "income", welfare_type))]

  expect_error(metadata_validate_output(metadata))

})

test_that("metadata_validate_output() works identifying type/ formating error", {

  metadata <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  metadata[, `:=` (year = as.character(year),
              survey_year = as.character(survey_year))]

  expect_error(metadata_validate_output(metadata))

})

test_that("metadata_validate_output() works identifying invalid value", {

  metadata <- load_aux(
    maindir = temp_fld, #gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  metadata[, survey_coverage := fifelse(survey_coverage == "national",
                                  "national1", survey_coverage)]

  expect_error(metadata_validate_output(metadata))

})
