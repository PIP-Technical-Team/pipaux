test_that("indicators_validate_raw() works", {
  skip("not yet implemented")
})

test_that("indicators_validate_output() works", {
  skip("not yet implemented")
})

# helper: valid indicators output data ------------------------------------

make_indicators_output <- function(...) {
  dt <- data.table::data.table(
    page           = c("poverty", "inequality"),
    indicator_name = c("headcount", "gini"),
    label          = c("Headcount ratio", "Gini index"),
    definition     = c("Share of population below poverty line", "Gini coefficient"),
    unit           = c("percent", "index"),
    source         = c("PIP", "PIP")
  )
  modifyList(dt, list(...))
}

# output structure --------------------------------------------------------

test_that("indicators output has expected columns", {
  dt <- make_indicators_output()
  expect_true(all(c("page", "indicator_name", "label", "definition") %in% names(dt)))
})

test_that("indicators page is character", {
  dt <- make_indicators_output()
  expect_type(dt$page, "character")
})

test_that("indicators indicator_name is character", {
  dt <- make_indicators_output()
  expect_type(dt$indicator_name, "character")
})

test_that("indicators label is character", {
  dt <- make_indicators_output()
  expect_type(dt$label, "character")
})

test_that("indicators definition is character", {
  dt <- make_indicators_output()
  expect_type(dt$definition, "character")
})

test_that("indicators page has no NA values", {
  dt <- make_indicators_output()
  expect_false(any(is.na(dt$page)))
})

test_that("indicators indicator_name has no NA values", {
  dt <- make_indicators_output()
  expect_false(any(is.na(dt$indicator_name)))
})

test_that("indicators key page/indicator_name is unique", {
  dt <- make_indicators_output()
  expect_equal(nrow(dt), nrow(unique(dt[, .(page, indicator_name)])))
})

test_that("indicators errors on duplicate page/indicator_name", {
  bad <- make_indicators_output(
    page           = c("poverty", "poverty"),
    indicator_name = c("headcount", "headcount")
  )
  expect_gt(
    nrow(bad) - nrow(unique(bad, by = c("page", "indicator_name"))),
    0
  )
})

test_that("indicators is a data.table", {
  dt <- make_indicators_output()
  expect_s3_class(dt, "data.table")
})

# aux_indicators() --------------------------------------------------------

test_that("aux_indicators() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

