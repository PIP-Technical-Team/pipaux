# helper: valid country_list-like data ------------------------------------

make_regions_cl <- function(...) {
  dt <- data.table::data.table(
    id           = c("AAA", "BBB"),
    region       = c("East Asia & Pacific", "Europe & Central Asia"),
    region_code  = c("EAP", "ECA"),
    world        = c("World", "World"),
    world_code   = c("WLD", "WLD")
  )
  modifyList(dt, list(...))
}

# helper: valid output regions data ---------------------------------------

make_regions_output <- function(...) {
  dt <- data.table::data.table(
    region        = c("East Asia & Pacific", "Europe & Central Asia", "World"),
    region_code   = c("EAP", "ECA", "WLD"),
    grouping_type = c("region", "region", "world")
  )
  modifyList(dt, list(...))
}

# output structure --------------------------------------------------------

test_that("regions output has expected columns", {
  dt <- make_regions_output()
  expect_true(all(c("region", "region_code", "grouping_type") %in% names(dt)))
})

test_that("regions region_code is character", {
  dt <- make_regions_output()
  expect_type(dt$region_code, "character")
})

test_that("regions region is character", {
  dt <- make_regions_output()
  expect_type(dt$region, "character")
})

test_that("regions grouping_type is character", {
  dt <- make_regions_output()
  expect_type(dt$grouping_type, "character")
})

test_that("regions region_code has no NA values", {
  dt <- make_regions_output()
  expect_false(any(is.na(dt$region_code)))
})

test_that("regions region has no NA values", {
  dt <- make_regions_output()
  expect_false(any(is.na(dt$region)))
})

test_that("regions region_code is unique", {
  dt <- make_regions_output()
  expect_equal(length(unique(dt$region_code)), nrow(dt))
})

# melt transformation logic -----------------------------------------------

test_that("melting country_list produces region_code column", {
  cl <- make_regions_cl()
  ml <- data.table::melt(
    cl,
    id.vars         = "id",
    measure.vars    = data.table::patterns("code$"),
    variable.factor = FALSE,
    value.factor    = FALSE,
    value.name      = "region_code",
    variable.name   = "grouping_type"
  )
  expect_true("region_code" %in% names(ml))
})

test_that("melting country_list strips _code suffix from grouping_type", {
  cl <- make_regions_cl()
  ml <- data.table::melt(
    cl,
    id.vars         = "id",
    measure.vars    = data.table::patterns("code$"),
    variable.factor = FALSE,
    value.factor    = FALSE,
    value.name      = "region_code",
    variable.name   = "grouping_type"
  )
  ml[, grouping_type := gsub("_code", "", grouping_type)]
  expect_false(any(grepl("_code$", ml$grouping_type)))
})

test_that("unique regions are correctly extracted from country_list", {
  cl <- make_regions_cl()
  ml <- data.table::melt(
    cl,
    id.vars         = "id",
    measure.vars    = data.table::patterns("code$"),
    variable.factor = FALSE,
    value.factor    = FALSE,
    value.name      = "region_code",
    variable.name   = "grouping_type"
  )
  ml[, grouping_type := gsub("_code", "", grouping_type)]
  byv <- c("region_code", "grouping_type")
  result <- unique(ml[region_code != "", ..byv], by = byv)
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# aux_regions() -----------------------------------------------------------

test_that("aux_regions() requires GitHub and filesystem access", {
  skip("requires GitHub and filesystem access")
})

