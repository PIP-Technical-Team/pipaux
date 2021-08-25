test_that("aux data is up to date", {
  skip_on_ci()
  lf <- lsf.str("package:pipaux", pattern = "^pip_[a-z]{3}$")
  lf <- as.character(lf)

  for (i in seq_along(lf)) {
    # print(lf[i])
    z <- get(lf[i])
    expect_false(suppressWarnings(z(action = "update")))
  }

})
