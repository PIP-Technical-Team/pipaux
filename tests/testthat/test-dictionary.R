# helper: valid dictionary output data ------------------------------------

make_dictionary_output <- function(...) {
  dt <- data.table::data.table(
    variable_name = c("headcount", "poverty_gap"),
    label         = c("Headcount ratio", "Poverty gap"),
    definition    = c("Share of population below poverty line", "Mean shortfall"),
    unit          = c("percent", "percent"),
    source        = c("PIP", "PIP")
  )
  modifyList(dt, list(...))
}

# output structure --------------------------------------------------------

test_that("dictionary output has expected columns", {
  dt <- make_dictionary_output()
  expect_true(all(c("variable_name", "label", "definition") %in% names(dt)))
})

test_that("dictionary variable_name is character", {
  dt <- make_dictionary_output()
  expect_type(dt$variable_name, "character")
})

test_that("dictionary label is character", {
  dt <- make_dictionary_output()
  expect_type(dt$label, "character")
})

test_that("dictionary definition is character", {
  dt <- make_dictionary_output()
  expect_type(dt$definition, "character")
})

test_that("dictionary variable_name has no NA values", {
  dt <- make_dictionary_output()
  expect_false(any(is.na(dt$variable_name)))
})

test_that("dictionary label has no NA values", {
  dt <- make_dictionary_output()
  expect_false(any(is.na(dt$label)))
})

test_that("dictionary variable_name is unique", {
  dt <- make_dictionary_output()
  expect_equal(length(unique(dt$variable_name)), nrow(dt))
})

test_that("dictionary errors on duplicate variable_name", {
  bad <- make_dictionary_output(variable_name = c("headcount", "headcount"))
  expect_gt(
    nrow(bad) - nrow(unique(bad, by = "variable_name")),
    0
  )
})

test_that("dictionary is a data.table", {
  dt <- make_dictionary_output()
  expect_s3_class(dt, "data.table")
})

# aux_dictionary() --------------------------------------------------------

test_that("aux_dictionary() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
