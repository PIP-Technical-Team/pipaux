# pipfun::load_raw_aux is deprecated

    Code
      lr <- pipfun::load_from_gh(measure = "cpi")
      lf <- pipfun::load_from_gh(measure = "cpi")
      expect_equal(lr, lf, ignore_attr = TRUE)

