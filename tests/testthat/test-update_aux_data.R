test_that("is_top_level() returns FALSE when called inside a function", {
  result <- (function() is_top_level())()
  expect_false(result)
})

test_that("is_top_level() returns a single logical value", {
  result <- (function() is_top_level())()
  expect_type(result, "logical")
  expect_length(result, 1)
})

# resolve_measure_repo_owner() --------------------------------------------

test_that("resolve_measure_repo_owner() returns default repo and owner for standard measure", {
  result <- resolve_measure_repo_owner("cpi", "aux_cpi", "myowner")
  expect_equal(result$repo, "aux_cpi")
  expect_equal(result$owner, "myowner")
})

test_that("resolve_measure_repo_owner() resolves 'income_groups' to repo 'Class'", {
  result <- resolve_measure_repo_owner("income_groups", "aux_income_groups", "myowner")
  expect_equal(result$repo, "Class")
  expect_equal(result$owner, "myowner")
})

test_that("resolve_measure_repo_owner() resolves 'country_list' to repo 'Class'", {
  result <- resolve_measure_repo_owner("country_list", "aux_country_list", "myowner")
  expect_equal(result$repo, "Class")
  expect_equal(result$owner, "myowner")
})

test_that("resolve_measure_repo_owner() resolves 'nan' to owner 'PIP-Technical-Team'", {
  result <- resolve_measure_repo_owner("nan", "aux_nan", "myowner")
  expect_equal(result$repo, "aux_nan")
  expect_equal(result$owner, "PIP-Technical-Team")
})

test_that("resolve_measure_repo_owner() applies both overrides independently", {
  # nan does not trigger Class repo
  result_nan <- resolve_measure_repo_owner("nan", "aux_nan", "myowner")
  expect_equal(result_nan$repo, "aux_nan")

  # income_groups does not trigger PIP-Technical-Team owner
  result_ig <- resolve_measure_repo_owner("income_groups", "aux_income_groups", "myowner")
  expect_equal(result_ig$owner, "myowner")
})

test_that("resolve_measure_repo_owner() returns a named list with repo and owner", {
  result <- resolve_measure_repo_owner("ppp", "aux_ppp", "someowner")
  expect_type(result, "list")
  expect_named(result, c("repo", "owner"))
})

# Functions requiring external resources ----------------------------------

test_that("process_dependencies() requires GitHub access", {
  skip("requires GitHub access and pipfun")
})

test_that("execute_update() requires GitHub and pipfun", {
  skip("requires GitHub and pipfun")
})

test_that("aux_fun() requires GitHub and working release environment", {
  skip("requires GitHub, pipfun, and working release environment")
})

test_that("update_aux_measures() requires GitHub and working release environment", {
  skip("requires GitHub, pipfun, and working release environment")
})

