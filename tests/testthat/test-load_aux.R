test_that("data loads", {
  skip_on_ci()
  lf <- lsf.str("package:pipaux", pattern = "^pip_[a-z]{3}$")
  lf <- as.character(lf)
  lf <- setdiff(lf, "pip_wdi")

  laux <- function(x) {
    y <- gsub("pip_", "", x)
    z <- get(x)
    expect_equal(z("load"), load_aux(y))
  }

  purrr::walk(lf, laux)
})
