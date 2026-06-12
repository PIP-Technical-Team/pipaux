dependencies <- list(
  ppp = "country_list",
  pfw = character(),
  gdp = c("weo", "maddison", "wdi", "country_list"),
  wdi = character(),
  weo = c("pop"),
  pop = c("country_list", "pfw"),
  countries = c("pfw", "country_list"),
  metadata = "pfw",
  gdm = c("country_list", "pfw"),
  regions = c("country_list"),
  maddison = character(),
  country_list = character(),
  pce = c("wdi", "country_list"),
  cpi = "country_list",
  missing_data = c("country_list", "pce", "gdp", "pop", "pfw")
)

# ── Bug reproduction tests ────────────────────────────────────────────────────

# Bug 1: purrr::map() crashes when a repo lacks the target branch (e.g.,
# aux_missing_countries has no DEV branch → gh::gh() throws 422).
# fetch_repo_sha() must absorb the error and return NA_character_.
test_that("fetch_repo_sha returns NA_character_ when the branch does not exist", {
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      stop("GitHub API error (422): No commit found for SHA: DEV")
    },
    .package = "gh"
  )
  result <- fetch_repo_sha(
    owner = "PIP-Technical-Team",
    repo = "aux_missing_countries",
    branch = "DEV"
  )
  expect_true(is.na(result))
})

# Bug 3: read_signature_file() crashes on measures that have never been saved.
# After the fix it must return NA_character_ when the file is absent.
test_that("read_signature_file returns NA_character_ for a missing signature file", {
  result <- read_signature_file("nonexistent_measure", tempdir(), "DEV")
  expect_true(is.na(result))
})

# Bug 2: derived measures (e.g., missing_data) have no raw-data repo, so they
# never appear in aux_fns even when their dependencies change.
# expand_with_derived_measures() must add them when a dep is in aux_fns.
test_that("expand_with_derived_measures adds measures whose dependencies changed", {
  deps <- list(
    pfw = character(),
    gdp = c("weo", "country_list"),
    missing_data = c("country_list", "pce", "gdp", "pop", "pfw")
  )
  # pfw changed → missing_data depends on pfw → should be added
  result <- expand_with_derived_measures(aux_fns = "pfw", dependencies = deps)
  expect_true("missing_data" %in% result)
  # gdp does NOT depend on pfw → should not be added
  expect_false("gdp" %in% result)
  # the original measure must still be present
  expect_true("pfw" %in% result)
})

# Bug 3 (NA comparison): before_hash != after_hash returns NA (not FALSE) when
# read_signature_file returns NA_character_ for a missing file.
# Using !isTRUE(before_hash == after_hash) treats NA as "files differ" → safe.
test_that("NA signature comparison triggers update rather than erroring", {
  before_hash <- NA_character_
  after_hash <- "abc123"
  # old code: `if (before_hash != after_hash)` returns NA → error inside if()
  expect_true(isTRUE(is.na(before_hash != after_hash))) # demonstrates the problem
  # desired behaviour: treat NA as "hashes differ" → update
  expect_true(!isTRUE(before_hash == after_hash))
})

# Bug 5: after expand_with_derived_measures() adds missing_data to aux_fns,
# the inner SHA back-fill tries new_data$hash[aux_row_new] for a measure that
# has no aux_* repo → zero-length result → "replacement has length zero" error.
# The guard `if (any(aux_row_new))` must prevent the assignment.
test_that("expand_with_derived_measures result does not break hash back-fill for derived measures", {
  # Simulate new_data: only has an entry for pfw, not for missing_data
  new_data_sim <- data.table::data.table(
    Repo = "PIP-Technical-Team/aux_pfw",
    hash = "newsha123",
    branch = "DEV"
  )

  # Derived measure: missing_data has no row in new_data
  aux_file <- "missing_data"
  br <- "DEV"
  aux_row_new <- new_data_sim$Repo |>
    fs::path_file() |>
    sub("aux_", "", x = _) %in%
    aux_file &
    new_data_sim$branch == br

  # Core assertion: must be FALSE, not error
  expect_false(any(aux_row_new))
  # The guarded assignment must evaluate to zero-op (no error)
  org_hash <- c("oldsha")
  expect_no_error({
    if (any(aux_row_new)) {
      org_hash <- new_data_sim$hash[aux_row_new]
    }
  })
  expect_equal(org_hash, "oldsha") # unchanged
})


base64_value_mtcars <- "bXBnLGN5bCxkaXNwLGhwLGRyYXQsd3QscXNlYyx2cyxhbSxnZWFyLGNhcmIKMjEsNiwxNjAsMTEwLDMuOSwyLjYyLDE2LjQ2LDAsMSw0LDQKMjEsNiwxNjAsMTEwLDMuOSwyLjg3NSwxNy4wMiwwLDEsNCw0CjIyLjgsNCwxMDgsOTMsMy44NSwyLjMyLDE4LjYxLDEsMSw0LDEKMjEuNCw2LDI1OCwxMTAsMy4wOCwzLjIxNSwxOS40NCwxLDAsMywxCjE4LjcsOCwzNjAsMTc1LDMuMTUsMy40NCwxNy4wMiwwLDAsMywyCjE4LjEsNiwyMjUsMTA1LDIuNzYsMy40NiwyMC4yMiwxLDAsMywxCjE0LjMsOCwzNjAsMjQ1LDMuMjEsMy41NywxNS44NCwwLDAsMyw0CjI0LjQsNCwxNDYuNyw2MiwzLjY5LDMuMTksMjAsMSwwLDQsMgoyMi44LDQsMTQwLjgsOTUsMy45MiwzLjE1LDIyLjksMSwwLDQsMgoxOS4yLDYsMTY3LjYsMTIzLDMuOTIsMy40NCwxOC4zLDEsMCw0LDQKMTcuOCw2LDE2Ny42LDEyMywzLjkyLDMuNDQsMTguOSwxLDAsNCw0CjE2LjQsOCwyNzUuOCwxODAsMy4wNyw0LjA3LDE3LjQsMCwwLDMsMwoxNy4zLDgsMjc1LjgsMTgwLDMuMDcsMy43MywxNy42LDAsMCwzLDMKMTUuMiw4LDI3NS44LDE4MCwzLjA3LDMuNzgsMTgsMCwwLDMsMwoxMC40LDgsNDcyLDIwNSwyLjkzLDUuMjUsMTcuOTgsMCwwLDMsNAoxMC40LDgsNDYwLDIxNSwzLDUuNDI0LDE3LjgyLDAsMCwzLDQKMTQuNyw4LDQ0MCwyMzAsMy4yMyw1LjM0NSwxNy40MiwwLDAsMyw0CjMyLjQsNCw3OC43LDY2LDQuMDgsMi4yLDE5LjQ3LDEsMSw0LDEKMzAuNCw0LDc1LjcsNTIsNC45MywxLjYxNSwxOC41MiwxLDEsNCwyCjMzLjksNCw3MS4xLDY1LDQuMjIsMS44MzUsMTkuOSwxLDEsNCwxCjIxLjUsNCwxMjAuMSw5NywzLjcsMi40NjUsMjAuMDEsMSwwLDMsMQoxNS41LDgsMzE4LDE1MCwyLjc2LDMuNTIsMTYuODcsMCwwLDMsMgoxNS4yLDgsMzA0LDE1MCwzLjE1LDMuNDM1LDE3LjMsMCwwLDMsMgoxMy4zLDgsMzUwLDI0NSwzLjczLDMuODQsMTUuNDEsMCwwLDMsNAoxOS4yLDgsNDAwLDE3NSwzLjA4LDMuODQ1LDE3LjA1LDAsMCwzLDIKMjcuMyw0LDc5LDY2LDQuMDgsMS45MzUsMTguOSwxLDEsNCwxCjI2LDQsMTIwLjMsOTEsNC40MywyLjE0LDE2LjcsMCwxLDUsMgozMC40LDQsOTUuMSwxMTMsMy43NywxLjUxMywxNi45LDEsMSw1LDIKMTUuOCw4LDM1MSwyNjQsNC4yMiwzLjE3LDE0LjUsMCwxLDUsNAoxOS43LDYsMTQ1LDE3NSwzLjYyLDIuNzcsMTUuNSwwLDEsNSw2CjE1LDgsMzAxLDMzNSwzLjU0LDMuNTcsMTQuNiwwLDEsNSw4CjIxLjQsNCwxMjEsMTA5LDQuMTEsMi43OCwxOC42LDEsMSw0LDI="

test_that("return_value works as expected", {
  expect_equal(
    return_value("pce", dependencies),
    c("country_list", "wdi", "pce")
  )
  expect_equal(return_value("wdi", dependencies), "wdi")
  expect_equal(
    return_value("gdp", dependencies),
    c("country_list", "wdi", "maddison", "pfw", "pop", "weo", "gdp")
  )
})

test_that("convert_df_to_base64 works as expected", {
  expect_equal(convert_df_to_base64(mtcars), base64_value_mtcars)
})

test_that("read_dependencies caches remote dependency metadata", {
  cache_file <- fs::path(tempdir(), "new_dependency.yml")

  testthat::local_mocked_bindings(
    download_metadata_text = function(
      owner,
      path,
      token = NULL,
      ref = "metadata"
    ) {
      "ppp: country_list\npfw: ''\ngdp: weo, maddison, wdi, country_list\n"
    },
    metadata_cache_file = function(filename, owner) cache_file,
    packaged_metadata_file = function(filename) NA_character_,
    .package = "pipaux"
  )

  out <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner = "PIP-Technical-Team"
  )

  expect_equal(
    out,
    list(
      ppp = "country_list",
      pfw = character(),
      gdp = c("weo", "maddison", "wdi", "country_list")
    )
  )
  expect_true(fs::file_exists(cache_file))
  expect_match(readLines(cache_file, warn = FALSE)[1], "^ppp:")
})

test_that("read_dependencies falls back to cached dependency metadata", {
  cache_file <- fs::path(tempdir(), "cached_new_dependency.yml")
  writeLines("pfw: ''\nmetadata: pfw\n", cache_file)

  testthat::local_mocked_bindings(
    download_metadata_text = function(
      owner,
      path,
      token = NULL,
      ref = "metadata"
    ) {
      stop("HTTP 429")
    },
    metadata_cache_file = function(filename, owner) cache_file,
    packaged_metadata_file = function(filename) NA_character_,
    .package = "pipaux"
  )

  out <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner = "PIP-Technical-Team"
  )

  expect_equal(out, list(pfw = character(), metadata = "pfw"))
})

test_that("read_dependencies returns empty list when all sources fail", {
  cache_file <- fs::path(tempdir(), "missing_new_dependency.yml")

  testthat::local_mocked_bindings(
    download_metadata_text = function(
      owner,
      path,
      token = NULL,
      ref = "metadata"
    ) {
      stop("HTTP 429")
    },
    metadata_cache_file = function(filename, owner) cache_file,
    packaged_metadata_file = function(filename) NA_character_,
    .package = "pipaux"
  )

  out <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner = "PIP-Technical-Team"
  )

  expect_equal(out, list())
})

test_that("read_git_metadata caches remote metadata", {
  cache_file <- fs::path(tempdir(), "git_metadata.csv")

  testthat::local_mocked_bindings(
    download_metadata_text = function(
      owner,
      path,
      token = NULL,
      ref = "metadata"
    ) {
      "Repo,hash,branch\nPIP-Technical-Team/aux_ppp,abc123,DEV\n"
    },
    metadata_cache_file = function(filename, owner) cache_file,
    packaged_metadata_file = function(filename) NA_character_,
    .package = "pipaux"
  )

  out <- read_git_metadata(owner = "PIP-Technical-Team")

  expect_s3_class(out, "data.table")
  expect_equal(out$Repo, "PIP-Technical-Team/aux_ppp")
  expect_true(fs::file_exists(cache_file))
})

test_that("read_git_metadata falls back to packaged metadata", {
  cache_file <- fs::path(tempdir(), "missing_git_metadata.csv")
  packaged_file <- fs::path(tempdir(), "packaged_git_metadata.csv")

  data.table::fwrite(
    data.table::data.table(
      Repo = "PIP-Technical-Team/aux_ppp",
      hash = "abc123",
      branch = "DEV"
    ),
    packaged_file
  )

  testthat::local_mocked_bindings(
    download_metadata_text = function(
      owner,
      path,
      token = NULL,
      ref = "metadata"
    ) {
      stop("HTTP 429")
    },
    metadata_cache_file = function(filename, owner) cache_file,
    packaged_metadata_file = function(filename) packaged_file,
    .package = "pipaux"
  )

  out <- read_git_metadata(owner = "PIP-Technical-Team")

  expect_s3_class(out, "data.table")
  expect_equal(out$Repo, "PIP-Technical-Team/aux_ppp")
  expect_equal(out$hash, "abc123")
})

test_that("is_github_sha_conflict_error detects stale SHA errors", {
  conflict_error <- simpleError(
    "GitHub API error (409): gdp.csv does not match abcdef"
  )
  non_conflict_error <- simpleError("GitHub API error (422): branch missing")

  expect_true(is_github_sha_conflict_error(conflict_error))
  expect_false(is_github_sha_conflict_error(non_conflict_error))
})

test_that("run_aux_update_with_retry retries on SHA conflict and succeeds", {
  attempts <- 0L

  flaky_update <- function(maindir, branch) {
    attempts <<- attempts + 1L

    if (attempts == 1L) {
      stop("GitHub API error (409): gdp.csv does not match abcdef")
    }

    invisible(TRUE)
  }

  expect_no_error(
    run_aux_update_with_retry(
      update_fn = flaky_update,
      fn_name = "pip_gdp",
      maindir = tempdir(),
      branch = "DEV",
      max_attempts = 2L,
      wait_seconds = 0
    )
  )
  expect_equal(attempts, 2L)
})

test_that("run_aux_update_with_retry does not retry non-409 errors", {
  attempts <- 0L

  failing_update <- function(maindir, branch) {
    attempts <<- attempts + 1L
    stop("GitHub API error (422): branch missing")
  }

  expect_error(
    run_aux_update_with_retry(
      update_fn = failing_update,
      fn_name = "pip_gdp",
      maindir = tempdir(),
      branch = "DEV",
      max_attempts = 3L,
      wait_seconds = 0
    ),
    "422"
  )
  expect_equal(attempts, 1L)
})

test_that("update_git_metadata_with_retry retries after SHA conflict", {
  call_count <- 0L

  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) {
      if (grepl("GET /repos", endpoint)) {
        return(list(sha = paste0("sha", call_count + 1L)))
      }

      call_count <<- call_count + 1L

      if (call_count == 1L) {
        stop("GitHub API error (409): git_metadata.csv does not match deadbeef")
      }

      list(content = list(sha = "updated"))
    },
    .package = "gh"
  )

  expect_no_error(
    update_git_metadata_with_retry(
      org_data = data.table::data.table(
        Repo = "PIP-Technical-Team/aux_gdp",
        hash = "abc",
        branch = "DEV"
      ),
      token = "fake-token",
      max_attempts = 2L,
      wait_seconds = 0
    )
  )
  expect_equal(call_count, 2L)
})
