# pipfun::pip_sign_save is deprecated

    Code
      tdir <- fs::path_temp("pipfun-l")
      lx <- list(x = 1)
      measure <- "ltst"
      saved <- pipfun::pip_sign_save(x = lx, measure = measure, msrdir = tdir,
        save_dta = TRUE)
    Message <cliMessage>
      ! Data signature has changed
      'ltst.rds' has been updated
    Code
      expect_true(saved)
