
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "sna"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("sna_validate_raw() works identifying type/ formating error", {

  sna_fy <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    filename = "sna_metadata",
    ext    = "csv"
  )

  sna_fy[, Day := as.character(Day)]

  expect_error(sna_fy_validate_raw(sna_fy))

})
