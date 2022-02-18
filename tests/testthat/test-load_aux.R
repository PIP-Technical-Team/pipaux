test_that("data loads", {
  skip_on_ci()
  lf <- lsf.str("package:pipaux", pattern = "^pip_[a-z]{3}$")
  lf <- as.character(lf)

  laux <- function(x) {
    y <- gsub("pip_", "", x)
    z <- get(x)
    expect_equal(z("load"), load_aux(y))
  }

  purrr::walk(lf, laux)
})
