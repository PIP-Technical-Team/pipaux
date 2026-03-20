test_that("cl_validate_raw() works", {
  # helper: valid raw country list data -------------------------------------

  make_cl_raw <- function(...) {
    dt <- data.table::data.table(
      country_code      = c("AAA", "BBB"),
      country_name      = c("Country A", "Country B"),
      africa_split      = c("Eastern and Southern Africa", NA_character_),
      africa_split_code = c("AFE", NA_character_),
      regionpcn         = c("Sub-Saharan Africa", "Europe & Central Asia"),
      regionpcn_code    = c("SSA", "ECA"),
      region            = c("Sub-Saharan Africa", "Europe & Central Asia"),
      region_code       = c("SSF", "ECS"),
      world             = c("World", "World"),
      world_code        = c("WLD", "WLD")
    )
    modifyList(dt, list(...))
  }

  # cl_validate_raw() ------------------------------------------------------

  test_that("cl_validate_raw() errors when country_code is not character", {
    bad <- make_cl_raw(country_code = c(1, 2))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when country_name is not character", {
    bad <- make_cl_raw(country_name = c(1, 2))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when africa_split has invalid value", {
    bad <- make_cl_raw(africa_split = c("Eastern and Southern Africa", "Invalid"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when africa_split_code has invalid value", {
    bad <- make_cl_raw(africa_split_code = c("AFE", "INVALID"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when regionpcn_code has invalid value", {
    bad <- make_cl_raw(regionpcn_code = c("SSA", "INVALID"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when region_code has invalid value", {
    bad <- make_cl_raw(region_code = c("SSF", "INVALID"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when world has invalid value", {
    bad <- make_cl_raw(world = c("World", "Invalid"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when world_code has invalid value", {
    bad <- make_cl_raw(world_code = c("WLD", "INVALID"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when country_code is NA", {
    bad <- make_cl_raw(country_code = c("AAA", NA_character_))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors on duplicate country_code", {
    bad <- make_cl_raw(country_code = c("AAA", "AAA"))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when regionpcn is not character", {
    bad <- make_cl_raw(regionpcn = c(1, 2))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when regionpcn_code is not character", {
    bad <- make_cl_raw(regionpcn_code = c(1, 2))
    expect_error(cl_validate_raw(cl = bad, detail = FALSE))
  })

  test_that("cl_validate_raw() errors when data is NULL", {
    expect_error(cl_validate_raw(cl = NULL, detail = FALSE))
  })

  # aux_country_list() ------------------------------------------------------

  test_that("aux_country_list() requires GitHub and filesystem access", {
    skip("requires GitHub and filesystem access")
  })
})

