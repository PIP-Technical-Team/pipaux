
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "maddison"
gls <- pipfun::pip_create_globals()
temp_fld <- "Y:/tefera_pipaux_test"

test_that("mpd_validate_raw() works identifying type/ formating error", {

  mpd <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  mpd[, `:=` (year = as.character(year),
              mpd_gdp = as.character(mpd_gdp))]

  expect_error(mpd_validate_raw(mpd))

})
