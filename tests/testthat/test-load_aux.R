context("all datasets load fine")

test_that("data loads", {

  lf <- lsf.str("package:pipaux", pattern = "^pip_[a-z]{3}$")
  lf <- as.character(lf)

  laux <- function(x) {
    y <- gsub("pip_", "", x)
    z <- get(x)
    expect_equal(z("load"), pipaux::load_aux(y))
  }

  purrr::walk(lf, laux)

})
