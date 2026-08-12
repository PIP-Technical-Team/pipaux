

# helper: valid output income groups data ---------------------------------

make_incgroup_output <- function(...) {
  dt <- data.table::data.table(
    country_code       = c("AAA", "BBB"),
    year_data          = c(2010, 2011),
    income_group       = c("High income", "Low income"),
    income_group_code  = c("HIC", "LIC"),
    incgroup_historical = c("High income", "Low income"),
    fcv                = c("", ""),
    ssa_subregion_code = c("", "")
  )

  replacements <- list(...)
  if (length(replacements) > 0) {
    for (nm in names(replacements)) {
      dt[[nm]] <- replacements[[nm]]
    }
  }

  dt
}

# incgroup_validate_output() ----------------------------------------------


test_that("incgroup_validate_output() errors when income_group has invalid value", {
  bad <- make_incgroup_output(income_group = c("High income", "Invalid"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when income_group_code has invalid value", {
  bad <- make_incgroup_output(income_group_code = c("HIC", "INVALID"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when incgroup_historical has invalid value", {
  bad <- make_incgroup_output(incgroup_historical = c("High income", "Invalid"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() passes on valid input", {
  good <- make_incgroup_output()
  expect_no_error(incgroup_validate_output(incgroup = good, detail = FALSE))
})

test_that("incgroup_validate_output() errors when country_code is not character", {
  bad <- make_incgroup_output(country_code = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when year_data is not numeric", {
  bad <- make_incgroup_output(year_data = c("2010", "2011"))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when income_group is not character", {
  bad <- make_incgroup_output(income_group = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when income_group_code is not character", {
  bad <- make_incgroup_output(income_group_code = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when fcv is not character", {
  bad <- make_incgroup_output(fcv = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when ssa_subregion_code is not character", {
  bad <- make_incgroup_output(ssa_subregion_code = c(1, 2))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when country_code is NA", {
  bad <- make_incgroup_output(country_code = c("AAA", NA_character_))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when year_data is NA", {
  bad <- make_incgroup_output(year_data = c(2010, NA_real_))
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors on duplicate key values", {
  bad <- make_incgroup_output(
    country_code = c("AAA", "AAA"),
    year_data    = c(2010, 2010)
  )
  expect_error(incgroup_validate_output(incgroup = bad, detail = FALSE))
})

test_that("incgroup_validate_output() errors when data is NULL", {
  expect_error(incgroup_validate_output(incgroup = NULL, detail = FALSE))
})

# aux_income_groups() -----------------------------------------------------

test_that("aux_income_groups(action = 'load') delegates to pipload::load_aux_data", {
  called <- NULL

  testthat::local_mocked_bindings(
    load_aux_data = function(measure, verbose) {
      called <<- list(measure = measure, verbose = verbose)
      data.table::data.table(ok = 1L)
    },
    .package = "pipload"
  )

  out <- aux_income_groups(action = "load", verbose = TRUE)

  expect_identical(called$measure, "income_groups")
  expect_identical(called$verbose, TRUE)
  expect_s3_class(out, "data.table")
})

test_that("aux_income_groups(action = 'update') transforms, validates, and saves", {
  raw_dt <- data.table::data.table(
    code = c("AAA", "BBB"),
    year_data = c(2010, 2011),
    incgroup = c("High income", "Low income"),
    fcv = c("", ""),
    regionssa = c("", "")
  )
  attr(raw_dt, "gh") <- list(sha = "abc123")

  validated_dt <- NULL
  pip_aux_save_args <- NULL
  save_aux_args <- NULL

  testthat::local_mocked_bindings(
    get_from_auxenv = function(key) {
      if (identical(key, "wrk_release")) {
        return(list(release = "20260401", identity = "TEST"))
      }
      NULL
    },
    incgroup_validate_output = function(incgroup, detail) {
      validated_dt <<- data.table::copy(incgroup)
      invisible(NULL)
    },
    pip_aux_save = function(...) {
      pip_aux_save_args <<- list(...)
      TRUE
    },
    save_aux_to_gh = function(...) {
      save_aux_args <<- list(...)
      invisible(NULL)
    },
    .package = "pipaux"
  )

  testthat::local_mocked_bindings(
    load_from_gh = function(...) raw_dt,
    .package = "pipfun"
  )

  out <- aux_income_groups(
    action = "update",
    owner = "PIP-Technical-Team",
    detail = FALSE
  )

  expect_true(isTRUE(out))
  expect_true("country_code" %in% names(validated_dt))
  expect_true("incgroup_historical" %in% names(validated_dt))
  expect_true("income_group" %in% names(validated_dt))
  expect_true("income_group_code" %in% names(validated_dt))
  expect_identical(validated_dt$income_group_code, c("HIC", "LIC"))

  expect_identical(pip_aux_save_args$id, "income_groups")
  expect_identical(pip_aux_save_args$pk, c("country_code", "year"))
  expect_identical(pip_aux_save_args$code_label, "aux_income_groups")

  expect_identical(save_aux_args$owner, "PIP-Technical-Team")
  expect_identical(save_aux_args$measure, "income_groups")
  expect_identical(save_aux_args$repo, "aux_income_groups")
  expect_identical(save_aux_args$branch, "20260401_TEST")
  expect_identical(save_aux_args$filename, "income_groups")
})

