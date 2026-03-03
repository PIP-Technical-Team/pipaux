# check_github_status() ---------------------------------------------------

test_that("check_github_status() skips 'countries' measure without GitHub call", {
  result <- check_github_status(
    measure        = "countries",
    repo           = "aux_countries",
    owner          = "RossanaTat",
    release_branch = "20250101_TEST",
    verbose        = FALSE
  )
  expect_false(result$update_gh)
  expect_equal(result$gh_reason, "Skipped")
})

test_that("check_github_status() skips 'missing_data' measure without GitHub call", {
  result <- check_github_status(
    measure        = "missing_data",
    repo           = "aux_missing_data",
    owner          = "RossanaTat",
    release_branch = "20250101_TEST",
    verbose        = FALSE
  )
  expect_false(result$update_gh)
  expect_equal(result$gh_reason, "Skipped")
})

test_that("check_github_status() returns a list with update_gh and gh_reason", {
  result <- check_github_status(
    measure        = "countries",
    repo           = "aux_countries",
    owner          = "RossanaTat",
    release_branch = "20250101_TEST",
    verbose        = FALSE
  )
  expect_type(result, "list")
  expect_named(result, c("update_gh", "gh_reason"))
})

test_that("check_github_status() requires GitHub for non-skipped measures", {
  skip("requires GitHub access")
})

# check_y_drive_status() --------------------------------------------------

test_that("check_y_drive_status() requires filesystem access", {
  skip("requires pipload filesystem access")
})

# check_status() ----------------------------------------------------------

test_that("check_status() returns list with update_gh and update_y", {
  .pipaux$wrk_release <- list(release = "20250101", identity = "TEST")
  on.exit(.pipaux$wrk_release <- NULL)

  result <- check_status(
    measure = "countries",
    repo    = "aux_countries",
    owner   = "RossanaTat",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true(all(c("update_gh", "update_y") %in% names(result)))
})

test_that("check_status() includes reasons when include_reason = TRUE", {
  .pipaux$wrk_release <- list(release = "20250101", identity = "TEST")
  on.exit(.pipaux$wrk_release <- NULL)

  result <- check_status(
    measure        = "countries",
    repo           = "aux_countries",
    owner          = "RossanaTat",
    verbose        = FALSE,
    include_reason = TRUE
  )

  expect_true(all(c("update_gh", "update_y", "y_reason") %in% names(result)))
})

test_that("check_status() does not include reasons by default", {
  .pipaux$wrk_release <- list(release = "20250101", identity = "TEST")
  on.exit(.pipaux$wrk_release <- NULL)

  result <- check_status(
    measure = "countries",
    repo    = "aux_countries",
    owner   = "RossanaTat",
    verbose = FALSE
  )

  expect_false("gh_reason" %in% names(result))
  expect_false("y_reason" %in% names(result))
})

# get_fs_status() ---------------------------------------------------------

test_that("get_fs_status() returns same structure as check_status()", {
  .pipaux$wrk_release <- list(release = "20250101", identity = "TEST")
  on.exit(.pipaux$wrk_release <- NULL)

  result <- get_fs_status(
    measure = "countries",
    repo    = "aux_countries",
    owner   = "RossanaTat"
  )

  expect_type(result, "list")
  expect_true(all(c("update_gh", "update_y") %in% names(result)))
})

test_that("get_fs_status() requires GitHub for non-skipped measures", {
  skip("requires GitHub access")
})