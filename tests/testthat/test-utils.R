# get_from_auxenv() -------------------------------------------------------

test_that("get_from_auxenv() returns NULL for missing key", {
  expect_null(get_from_auxenv("nonexistent_key_xyz"))
})

test_that("get_from_auxenv() returns value for existing key", {
  .pipaux$test_key <- "test_value"
  on.exit(.pipaux$test_key <- NULL)
  expect_equal(get_from_auxenv("test_key"), "test_value")
})

test_that("get_from_auxenv() returns correct type for numeric value", {
  .pipaux$test_num <- 42L
  on.exit(.pipaux$test_num <- NULL)
  expect_identical(get_from_auxenv("test_num"), 42L)
})

# finalize_aux_log() ------------------------------------------------------

test_that("finalize_aux_log() removes active_aux_log from .piplogenv", {
  .piplogenv$active_aux_log <- "some_log"
  finalize_aux_log()
  expect_false(rlang::env_has(.piplogenv, "active_aux_log"))
})

test_that("finalize_aux_log() is a no-op when no active log exists", {
  if (rlang::env_has(.piplogenv, "active_aux_log")) {
    rlang::env_unbind(.piplogenv, "active_aux_log")
  }
  expect_no_error(finalize_aux_log())
})

# aux_log_last_name() -----------------------------------------------------

test_that("aux_log_last_name() returns the last log name", {
  .piplogenv$last_aux_log <- "pipaux_update_log"
  expect_equal(aux_log_last_name(), "pipaux_update_log")
})

# save_aux_to_gh() --------------------------------------------------------

test_that("save_aux_to_gh() requires GitHub access", {
  skip("requires GitHub access")
})

# pip_aux_save() ----------------------------------------------------------

test_that("pip_aux_save() requires pipload filesystem access", {
  skip("requires pipload filesystem access")
})

# last_item() -------------------------------------------------------------

test_that("last_item() returns single item unchanged", {
  expect_equal(last_item("one"), "one")
})

test_that("last_item() joins two items with 'and'", {
  expect_equal(last_item(c("one", "two")), "one and two")
})

test_that("last_item() joins three or more items with commas and 'and'", {
  expect_equal(last_item(c("one", "two", "three")), "one, two, and three")
})

test_that("last_item() uses custom word", {
  expect_equal(last_item(c("one", "two"), word = "or"), "one or two")
  expect_equal(last_item(c("one", "two", "three"), word = "or"), "one, two, or three")
})

test_that("last_item() coerces non-character input with warning", {
  expect_warning(last_item(1:3), "`x` must be character")
})

# n_decimals() ------------------------------------------------------------

test_that("n_decimals() returns 0 for integers", {
  expect_equal(n_decimals(1), 0)
  expect_equal(n_decimals(100), 0)
})

test_that("n_decimals() counts decimal places correctly", {
  expect_equal(n_decimals(1.5), 1)
  expect_equal(n_decimals(1.25), 2)
  expect_equal(n_decimals(1.123), 3)
})

test_that("n_decimals() works on a vector", {
  result <- n_decimals(c(1.5, 2.25, 3))
  expect_equal(result, c(1, 2, 0))
})

# get_month_number() ------------------------------------------------------

test_that("get_month_number() returns correct month numbers", {
  expect_equal(get_month_number("January"), 1)
  expect_equal(get_month_number("June"), 6)
  expect_equal(get_month_number("December"), 12)
})

test_that("get_month_number() works on a vector", {
  result <- get_month_number(c("March", "July", "November"))
  expect_equal(result, c(3, 7, 11))
})

# days_in_month() ---------------------------------------------------------

test_that("days_in_month() returns correct days for standard months", {
  expect_equal(days_in_month("January", 2020), 31)
  expect_equal(days_in_month("April", 2020), 30)
  expect_equal(days_in_month("November", 2020), 30)
})

test_that("days_in_month() handles leap years for February", {
  expect_equal(days_in_month("February", 2020), 29) # leap year
  expect_equal(days_in_month("February", 2019), 28) # non-leap year
  expect_equal(days_in_month("February", 2000), 29) # divisible by 400
  expect_equal(days_in_month("February", 1900), 28) # divisible by 100 not 400
})

test_that("days_in_month() returns NA for missing inputs", {
  expect_true(is.na(days_in_month(NA, 2020)))
  expect_true(is.na(days_in_month("January", NA)))
})

# chain() -----------------------------------------------------------------

test_that("chain() returns original vector when no NAs", {
  x <- c(100, 200, 300)
  y <- c(10, 20, 30)
  expect_equal(chain(x, y), x)
})

test_that("chain() returns replacement vector when all original is NA", {
  x <- c(NA_real_, NA_real_, NA_real_)
  y <- c(10, 20, 30)
  expect_equal(chain(x, y), y)
})

test_that("chain() returns original vector when all replacement is NA", {
  x <- c(100, NA_real_, 300)
  y <- c(NA_real_, NA_real_, NA_real_)
  expect_equal(chain(x, y), x)
})

test_that("chain() chains forward to fill NA gap", {
  # x[2] is NA, should be filled from x[1] * (y[2]/y[1])
  x <- c(100, NA_real_, NA_real_)
  y <- c(10, 20, 40)
  result <- chain(x, y)
  expect_equal(result[2], 200) # 100 * (20/10)
  expect_equal(result[3], 400) # 200 * (40/20)
})

test_that("chain() chains backward to fill NA gap", {
  # x[1] is NA, should be filled from x[2] * (y[1]/y[2])
  x <- c(NA_real_, 200, NA_real_)
  y <- c(10, 20, 40)
  result <- chain(x, y)
  expect_equal(result[1], 100) # 200 * (10/20)
  expect_equal(result[3], 400) # 200 * (40/20)
})

test_that("chain() errors on non-numeric inputs", {
  expect_error(chain("a", c(1, 2, 3)))
  expect_error(chain(c(1, 2, 3), "a"))
})

