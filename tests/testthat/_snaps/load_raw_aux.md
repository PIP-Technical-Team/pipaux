# pipfun::load_raw_aux is deprecated

    Code
      lr <- load_raw_aux(measure = "cpi")
    Warning <lifecycle_warning_deprecated>
      `load_raw_aux()` was deprecated in pipaux 0.1.0.9003.
      i Please use `pipfun::load_from_gh()` instead.
    Code
      lf <- pipfun::load_from_gh(measure = "cpi")
      expect_equal(lr, lf, ignore_attr = TRUE)

