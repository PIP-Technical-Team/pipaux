
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "sna"

test_that("sna_validate_raw() works identifying type/ formating error", {

  sna <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  sna[, `:=` (year = as.character(year),
              GDP = as.character(GDP))]

  expect_error(sna_validate_raw(sna))

})

test_that("sna_validate_raw() works identifying invalid value", {

  sna <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  sna[, coverage := fifelse(coverage == "National", "national1", coverage)]

  expect_error(sna_validate_raw(sna))

})
