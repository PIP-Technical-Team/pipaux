
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "wdi"

test_that("wdi_validate_raw() works identifying duplicate error", {

  wdi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  wdi[, `:=` (year = fifelse((year == 1960 & country_code == "ABW"),
                                 1961, year))]

  expect_error(wdi_validate_raw(wdi))

})

test_that("wdi_validate_raw() works identifying type/ formating error", {

  wdi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch
  )

  wdi[, `:=` (year = as.character(year),
              NE.CON.PRVT.PC.KD = as.character(NE.CON.PRVT.PC.KD),
              NY.GDP.PCAP.KD = as.character(NY.GDP.PCAP.KD))]

  expect_error(wdi_validate_raw(wdi))

})
