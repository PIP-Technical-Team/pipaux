test_that("pipfun::load_raw_aux is deprecated", {
  expect_snapshot({

    lr <- load_raw_aux(measure  = "cpi")
    lf <- pipfun::load_from_gh(measure = "cpi")

    expect_equal(lr, lf, ignore_attr = TRUE)

  })
})
