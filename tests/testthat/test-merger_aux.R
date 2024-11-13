temp_fld <- "Y:/tefera_pipaux_test"

pfw <- load_aux("pfw",
                branch = "DEV",
                maindir = temp_fld)

cpi <- load_aux("cpi",
                branch = "DEV",
                maindir = temp_fld)

cpi <- cpi[, -c("cpi_domain")]

ppp <- load_aux("ppp",
                branch = "DEV",
                maindir = temp_fld)

test_that("Merge pfw and cpi without erros", {

  expect_no_error(merger_aux(pfw, cpi))

})

test_that("Merge pfw and ppp without erros", {

  expect_no_error(merger_aux(pfw, ppp))

})
