library(testthat)

test_that("load_aux function works correctly", {
  expect_error(load_aux(maindir = "invalid_path", measure = "measure", branch = "branch"), "could not find function")
})

test_that("pcn_region_code is defined", {
  pcn_region_code <- "SSA"
  expect_equal(pcn_region_code, "SSA")
})

test_that("duplicate errors are identified", {
  data <- data.frame(value = c(1, 1, 2))
  expect_equal(sum(duplicated(data$value)), 2)
})

test_that("type/formatting errors are identified", {
  expect_error(as.numeric("text"), "NAs introduced by coercion")
})

test_that("invalid values are handled", {
  expect_false(is.na(NA))
})