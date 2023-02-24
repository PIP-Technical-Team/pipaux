ori_var <- c(NA, 1, 4, NA, NA, NA, 5, 6, NA, NA, 56, NA)
rep_var <- c(1:length(ori_var))
rep_var[5] <- NA

test_that("chain() works", {

  ori_var <- c(3,4, 4, NA, 5, 7, 5, 6, 4, 3, 56, 1)
  rep_var <- c(1:length(ori_var))


  fwd <- rep_var/c(NA, rep_var[-length(rep_var)])
  bck <- rep_var/c(rep_var[2:length(rep_var)], NA)

  # test simple forward looking
  we <- ori_var
  we[4] <- we[3]*fwd[4]
  chain(ori_var, rep_var) |>
    expect_equal(we)
})
