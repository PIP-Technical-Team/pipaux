# helper: minimal raw CP data table ---------------------------------------

make_cp_raw_table <- function(extra_cols = list(), ...) {
  dt <- data.table::data.table(
    country          = c("AAA", "BBB"),
    requestyear      = c(2010, 2011),
    welfaretype      = c("CONS", "INC"),
    coverage         = c("N", "U")
  )
  for (nm in names(extra_cols)) dt[[nm]] <- extra_cols[[nm]]
  modifyList(dt, list(...))
}

# clean_cp_names() --------------------------------------------------------

test_that("clean_cp_names() renames country to country_code", {
  dt <- make_cp_raw_table()
  result <- clean_cp_names(dt)
  expect_true("country_code" %in% names(result))
  expect_false("country" %in% names(result))
})

test_that("clean_cp_names() renames requestyear to reporting_year", {
  dt <- make_cp_raw_table()
  result <- clean_cp_names(dt)
  expect_true("reporting_year" %in% names(result))
  expect_false("requestyear" %in% names(result))
})

test_that("clean_cp_names() recodes welfaretype CONS to consumption", {
  dt <- make_cp_raw_table()
  result <- clean_cp_names(dt)
  expect_equal(result$welfare_type[1], "consumption")
})

test_that("clean_cp_names() recodes welfaretype INC to income", {
  dt <- make_cp_raw_table()
  result <- clean_cp_names(dt)
  expect_equal(result$welfare_type[2], "income")
})

test_that("clean_cp_names() recodes coverage N to national", {
  dt <- make_cp_raw_table()
  result <- clean_cp_names(dt)
  expect_equal(result$reporting_level[1], "national")
})

test_that("clean_cp_names() recodes coverage U to urban", {
  dt <- make_cp_raw_table(coverage = c("U", "R"))
  result <- clean_cp_names(dt)
  expect_equal(result$reporting_level[1], "urban")
  expect_equal(result$reporting_level[2], "rural")
})

test_that("clean_cp_names() lowercases all column names", {
  dt <- make_cp_raw_table(extra_cols = list(SI_POV_ALL = c(0.1, 0.2)))
  result <- clean_cp_names(dt)
  expect_true(all(names(result) == tolower(names(result))))
})

test_that("clean_cp_names() strips xyzd prefix pattern from names", {
  dt <- make_cp_raw_table(extra_cols = list(xyzdMxyzSI_POV_ALL = c(0.1, 0.2)))
  result <- clean_cp_names(dt)
  expect_false(any(grepl("xyzd", names(result))))
})

test_that("clean_cp_names() returns a data.table", {
  dt <- make_cp_raw_table()
  result <- clean_cp_names(dt)
  expect_s3_class(result, "data.table")
})

test_that("clean_cp_names() handles optional columns gracefully", {
  dt <- make_cp_raw_table(extra_cols = list(
    si_pov_gini = c(0.3, 0.4),
    pppyear     = c(2017, 2017)
  ))
  result <- clean_cp_names(dt)
  expect_true("gini" %in% names(result))
  expect_true("ppp_year" %in% names(result))
})

# aux_cp() / aux_cp_update() ----------------------------------------------

test_that("aux_cp() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})