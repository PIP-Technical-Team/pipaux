# load data using `pip_cpi()` from DEV branch

# data based on action = load
cpi_data <- pip_cpi(action = "load",
                    branch = "DEV")

# generate data as action = update and save it in Testing folder
pip_cpi(action = "update",
        branch = "DEV",
        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing")

# load data from Testing folder
cpi_update_data <- pip_cpi(action = "load",
                        maindir = "//w1wbgencifs01/pip/PIP-Data_Testing/pipaux_Testing",
                        branch = "DEV")


test_that("No duplicate records in PIP CPI data", {

  expect_equal(any(duplicated(cpi_data,
                              by = c("country_code", "cpi_year",
                                     "survey_acronym","cpi_data_level"))),
               FALSE)
})

test_that("complete rows across `country_code, cpi_year, survey_acronym, cpi_data_level`", {

  pointblank::expect_rows_complete(
    cpi_data,
    columns = vars(country_code, cpi_year, survey_acronym, cpi_data_level),
    threshold = 1
  )
})

# test_that("column schemas match", {
#
#   expect_col_schema_match(
#     cpi_data,
#     schema = col_schema(
#       cpi_data,
#       country_code = "character",
#       survey_acronym = "character"#,
#       # cpi_domain = "character",
#       # cpi_data_level = "character",
#       # cpi_id = "character",
#       # cpi_year = "integer",
#       # survey_year = "numeric",
#       # cpi = "numeric",
#       # ccf = "numeric",
#       # change_cpi2011 = "numeric",
#       # cpi_domain_value = "numeric",
#       # cpi2017_unadj = "numeric",
#       # cpi2011_unadj = "numeric",
#       # cpi2011 = "numeric",
#       # cpi2017 = "numeric",
#       # cpi2011_SM22 = "numeric",
#       # cpi2017_SM22 = "numeric",
#       # cpi2005 = "logical"
#     ),
#     threshold = 1
#   )
# })

test_that("Key variables data types in PIP CPI data", {

  expect_true(is.character(cpi_data[["country_code"]]))
  expect_true(is.numeric(cpi_data[["cpi_year"]]))
  expect_true(is.character(cpi_data[["cpi_data_level"]]))
  expect_true(is.character(cpi_data[["survey_acronym"]]))

  expect_true(is.numeric(cpi_data[["cpi_year"]]))
  expect_true(is.numeric(cpi_data[["cpi"]]))
  expect_true(is.numeric(cpi_data[["ccf"]]))
  expect_true(is.numeric(cpi_data[["change_cpi2011"]]))
  expect_true(is.character(cpi_data[["cpi_domain"]]))
  expect_true(is.numeric(cpi_data[["cpi_domain_value"]]))
  expect_true(is.numeric(cpi_data[["cpi2017_unadj"]]))
  expect_true(is.numeric(cpi_data[["cpi2011_unadj"]]))
  expect_true(is.numeric(cpi_data[["cpi2011"]]))
  expect_true(is.numeric(cpi_data[["cpi2017"]]))
  expect_true(is.numeric(cpi_data[["cpi2011_SM22"]]))
  expect_true(is.numeric(cpi_data[["cpi2017_SM22"]]))
  expect_true(is.logical(cpi_data[["cpi2005"]]))
  expect_true(is.character(cpi_data[["cpi_id"]]))
})

test_that("values in `country_code` should match the regular expression: `^[A-Z]{3}$`", {

  expect_match(
    cpi_data[["country_code"]],
    regex = "^[A-Z]{3}$"
  )
})

test_that("Variable labels in PIP CPI data", {

  expect_equal(attr(cpi_data[["cpi_domain"]], "label"),
               "CPI domain to join with microdata")
  expect_equal(attr(cpi_data[["cpi_data_level"]], "label"),
               "Values to use as keys to join with cpi_domain_var")
  expect_equal(attr(cpi_data[["ccf"]], "label"),
               "Currency conversion factor")
  expect_equal(attr(cpi_data[["cpi"]], "label"),
               "Consumer Price Index (Based on 2011).")
})


test_that("Compare variables generated using `load` and `update` parameters", {

  expect_equal(colnames(cpi_update_data), colnames(cpi_data))

})

test_that("Compare data generated using `load` and `update` parameters", {

  expect_equal(cpi_data, cpi_update_data, ignore_attr = TRUE)

})

test_that("both tables match", {

  pointblank::expect_tbl_match(
    cpi_data,
    tbl = cpi_update_data,
    threshold = 1
  )
})

###############################################################################
# check if all variables that should be in the cpi data are available in the raw dataset

# raw cpi data
cpi_raw <- pipfun::load_from_gh(measure = "cpi", branch = "DEV")

test_that("column `date` exists", {

  pointblank::expect_col_exists(
    cpi_raw,
    columns = dplyr::vars(code, year, ref_year, survname, cpi_domain,
                   cpi_data_level, change_cpi2011, cpi2017_unadj,
                   cpi2011_unadj, cpi2011, cpi2017, cpi2011_SM22,
                   cpi2017_SM22, cpi2005, change_cpi2011, cpi_id,
                   cur_adj),
    threshold = 1
  )
})


test_that("complete rows across `code, year, survname, cpi_data_level`", {

  pointblank::expect_rows_complete(
    cpi_raw,
    columns = dplyr::vars(code, year, survname, cpi_data_level),
    threshold = 1
  )
})

test_that("values in `code` should match the regular expression: `^[A-Z]{3}$`", {

  pointblank::expect_col_vals_regex(
    cpi_raw,
    columns = dplyr::vars(code),
    regex = "^[A-Z]{3}$",
    threshold = 1
  )
})

