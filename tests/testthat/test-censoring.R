path <- "../testdata/censored.xlsx"
dl <- list(
  countries =
    data.frame(country_code = character(0),
               survey_acronym = character(0),
               reporting_year = numeric(0),
               reporting_level = character(0),
               welfare_type = character(0),
               statistic = character(0)),
  regions =
    data.frame(region_code = logical(0),
               reporting_year = logical(0),
               statistic = logical(0)))

test_that("load_censoring() works", {
  dl <- load_censoring(path)
  expect_identical(names(dl), readxl::excel_sheets(path))
  expect_error(load_censoring("../testdata/censored.csv"))
})

test_that("transform_censoring() works", {
  dl1 <- load_censoring(path)
  dl2 <- transform_censoring(dl1)
  expect_identical(names(dl2), readxl::excel_sheets(path))
  expect_identical(
    dl2[[1]]$id,
    with(dl2[[1]],
         sprintf(
           "%s_%s_%s_%s_%s",
           country_code, reporting_year,
           survey_acronym, welfare_type,
           reporting_level
         )))
  expect_identical(
    dl2[[2]]$id,
    with(dl2[[2]],
         sprintf("%s_%s",
                 region_code, reporting_year
         )))
})

test_that("verify_input_censoring() works", {

  # Return as is if no errors
  expect_identical(verify_input_censoring(dl), dl)

  # Missing sheets
  tmp <- dl; tmp$countries <- NULL
  expect_error(verify_input_censoring(tmp))
  tmp <- dl; tmp$regions <- NULL
  expect_error(verify_input_censoring(tmp))

  # Missing columns
  tmp <- dl; tmp$countries$statistic <- NULL
  expect_error(verify_input_censoring(tmp))
  tmp <- dl; tmp$regions$region_code <- NULL
  expect_error(verify_input_censoring(tmp))

  # Incorrect class
  tmp <- dl
  tmp$countries$reporting_year <- as.character(tmp$countries$reporting_year)
  expect_error(verify_input_censoring(tmp))

})

test_that("verify_output_censoring() works", {

  # Create testdata
  dl$countries$id <- character(0)
  dl$regions$id <- character(0)

  # Return as is if no errors
  expect_identical(verify_output_censoring(dl), dl)

  # Missing sheets
  tmp <- dl; tmp$countries <- NULL
  expect_error(verify_output_censoring(tmp))
  tmp <- dl; tmp$regions <- NULL
  expect_error(verify_output_censoring(tmp))

  # Missing columns
  tmp <- dl; tmp$countries$statistic <- NULL
  expect_error(verify_output_censoring(tmp))
  tmp <- dl; tmp$regions$region_code <- NULL
  expect_error(verify_output_censoring(tmp))
  tmp <- dl; tmp$countries$id <- NULL
  expect_error(verify_output_censoring(tmp))
  tmp <- dl; tmp$regions$id <- NULL
  expect_error(verify_output_censoring(tmp))

  # Incorrect class
  tmp <- dl
  tmp$countries$reporting_year <- as.character(tmp$countries$reporting_year)
  expect_error(verify_input_censoring(tmp))

})
