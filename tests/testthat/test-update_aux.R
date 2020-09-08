context("Check all aux files are up to date")

test_that("aux data is up to date", {

  lf <- lsf.str("package:pipaux", pattern = "^pip_[a-z]{3}$")
  lf <- as.character(lf)

  laux <- function(x) {
    y <- gsub("pip_", "", x)
    z <- get(x)
    expect_false(suppressWarnings(z(action = "update")))
  }

  purrr::walk(lf, laux)
})
