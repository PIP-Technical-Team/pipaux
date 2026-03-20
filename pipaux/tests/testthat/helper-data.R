data <- data.frame(
  pcn_region_code = c("SSA", "SAR", "SSA", "SAR"),
  value = c(1, 2, 3, 4)
)

test_that("cl_validate_raw() works identifying invalid value", {
  result <- cl_validate_raw(data)
  expect_true(all(result$valid))
})

test_that("cl_validate_raw() works identifying duplicate error", {
  result <- cl_validate_raw(data)
  expect_equal(sum(result$duplicate), 0)
})

test_that("countries_validate_output() works identifying duplicate error", {
  result <- countries_validate_output(data)
  expect_equal(sum(result$duplicate), 0)
})

test_that("countries_validate_output() works identifying invalid value", {
  result <- countries_validate_output(data)
  expect_true(all(result$valid))
})

test_that("cpi_validate_output() works identifying duplicate error", {
  result <- cpi_validate_output(data)
  expect_equal(sum(result$duplicate), 0)
})

test_that("cpi_validate_output() works identifying type/formatting error", {
  result <- cpi_validate_output(data)
  expect_true(all(result$valid))
})

test_that("gdm_validate_output() works identifying duplicate error", {
  result <- gdm_validate_output(data)
  expect_equal(sum(result$duplicate), 0)
})

test_that("gdm_validate_output() works identifying type/formatting error", {
  result <- gdm_validate_output(data)
  expect_true(all(result$valid))
})

test_that("gdm_validate_output() works identifying invalid value", {
  result <- gdm_validate_output(data)
  expect_true(all(result$valid))
})