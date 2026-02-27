# helper: valid output pl data --------------------------------------------

make_pl_output <- function(...) {
  dt <- data.table::data.table(
    name         = c("1.00", "1.90", "2.15"),
    poverty_line = c(1.0, 1.9, 2.15),
    is_default   = c(FALSE, TRUE, FALSE),
    is_visible   = c(TRUE, TRUE, TRUE),
    ppp_year     = c(2011L, 2011L, 2017L)
  )
  modifyList(dt, list(...))
}

# helper: valid yaml-like list for aux_pl_clean() -------------------------

make_pl_list <- function(...) {
  l <- list(
    ranges    = list(
      list(min = 1.0, max = 2.0, increment = 0.5)
    ),
    default   = "1.5",
    visible   = c("1.00", "1.50", "2.00"),
    ppp_year  = 2017L
  )
  modifyList(l, list(...))
}

# pl_validate_output() ----------------------------------------------------

test_that("pl_validate_output() passes with valid data", {
  expect_no_error(pl_validate_output(pl = make_pl_output(), detail = FALSE))
})

test_that("pl_validate_output() errors when name is not character", {
  bad <- make_pl_output(name = c(1.00, 1.90, 2.15))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when poverty_line is not numeric", {
  bad <- make_pl_output(poverty_line = c("a", "b", "c"))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when is_default is not logical", {
  bad <- make_pl_output(is_default = c(0, 1, 0))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when is_visible is not logical", {
  bad <- make_pl_output(is_visible = c(0, 1, 1))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when ppp_year is not integer", {
  bad <- make_pl_output(ppp_year = c(2011.5, 2011.5, 2017.5))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when name is NA", {
  bad <- make_pl_output(name = c("1.00", NA_character_, "2.15"))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when ppp_year is NA", {
  bad <- make_pl_output(ppp_year = c(2011L, NA_integer_, 2017L))
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors on duplicate key values", {
  bad <- make_pl_output(
    name     = c("1.00", "1.00", "2.15"),
    ppp_year = c(2011L, 2011L, 2017L)
  )
  expect_error(pl_validate_output(pl = bad, detail = FALSE))
})

test_that("pl_validate_output() errors when data is NULL", {
  expect_error(pl_validate_output(pl = NULL, detail = FALSE))
})

# aux_pl_clean() ----------------------------------------------------------

test_that("aux_pl_clean() returns a data.table", {
  result <- aux_pl_clean(make_pl_list())
  expect_s3_class(result, "data.table")
})

test_that("aux_pl_clean() returns expected columns", {
  result <- aux_pl_clean(make_pl_list())
  expect_named(result, c("name", "poverty_line", "is_default", "is_visible", "ppp_year"))
})

test_that("aux_pl_clean() generates correct poverty lines from ranges", {
  result <- aux_pl_clean(make_pl_list())
  expect_equal(result$poverty_line, c(1.0, 1.5, 2.0))
})

test_that("aux_pl_clean() correctly identifies default poverty line", {
  result <- aux_pl_clean(make_pl_list())
  expect_equal(result$is_default, c(FALSE, TRUE, FALSE))
})

test_that("aux_pl_clean() correctly identifies visible poverty lines", {
  result <- aux_pl_clean(make_pl_list())
  expect_equal(result$is_visible, c(TRUE, TRUE, TRUE))
})

test_that("aux_pl_clean() ppp_year matches input", {
  result <- aux_pl_clean(make_pl_list())
  expect_true(all(result$ppp_year == 2017L))
})

test_that("aux_pl_clean() formats name with correct decimal places", {
  result <- aux_pl_clean(make_pl_list())
  expect_equal(result$name, c("1.00", "1.50", "2.00"))
})

test_that("aux_pl_clean() is_default and is_visible are logical", {
  result <- aux_pl_clean(make_pl_list())
  expect_type(result$is_default, "logical")
  expect_type(result$is_visible, "logical")
})

# aux_pl() ----------------------------------------------------------------

test_that("aux_pl() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

