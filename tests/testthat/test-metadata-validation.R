
## Initial parameters --------

branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "metadata"

test_that("metadata_validate_raw() works identifying duplicate error", {

  metadata <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  metadata[, `:=` (svy_id = fifelse(svy_id == "CNH_2005_URHS_v01_M",
                                 "CNH_2008_URHS_v01_M", svy_id))]

  expect_error(metadata_validate_raw(metadata), "Duplicate error")

})

test_that("metadata_validate_raw() works identifying type/ formating error", {

  metadata <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  metadata[, `:=` (year_start = as.character(year_start),
              year_end = as.character(year_end))]

  expect_error(metadata_validate_raw(metadata), "Type/ format error")

})

test_that("metadata_validate_raw() works identifying invalid value", {

  metadata <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  metadata[, reg := fifelse(reg == "SSA", "ssa", reg)]

  expect_error(metadata_validate_raw(metadata), "Invalid value in `reg`")

})

test_that("metadata_validate_output() works identifying duplicate error", {

  metadata <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  metadata[, `:=` (welfare_type = fifelse(welfare_type == "consumption",
                                       "income", welfare_type))]

  expect_error(metadata_validate_output(metadata), "Duplicate error")

})

test_that("metadata_validate_output() works identifying type/ formating error", {

  metadata <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  metadata[, `:=` (reporting_year = as.character(reporting_year),
              survey_year = as.character(survey_year))]

  expect_error(metadata_validate_output(metadata), "Type/ format error")

})

test_that("metadata_validate_output() works identifying invalid value", {

  metadata <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  metadata[, survey_coverage := fifelse(survey_coverage == "national",
                                  "national1", survey_coverage)]

  expect_error(metadata_validate_output(metadata), "Invalid value in `survey_coverage`")

})
