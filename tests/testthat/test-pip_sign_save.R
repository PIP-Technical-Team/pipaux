test_that("pipfun::pip_sign_save is deprecated", {
  expect_snapshot({
    tdir <- fs::path_temp("pipfun-l")

    lx <- list(x = 1)
    measure <- "ltst"

    saved <-
      pipfun::pip_sign_save(x = lx,
                    measure = measure,
                    msrdir = tdir,
                    save_dta = TRUE)

    expect_true(saved)
  })
})
