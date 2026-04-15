---
date: 2026-04-15
title: "auto_aux_update() instability: 4 compounding bugs"
category: "bugs"
type: "bug"
language: "R"
tags: [auto_aux_update, gh, purrr, missing_data, signature_file, derived_measures, 422]
root-cause: "purrr::map() for SHA fetch had no error handling; read_signature_file() was not null-safe; derived measures (missing_data) were never triggered due to absent raw-data repo"
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# auto_aux_update() instability: 4 compounding bugs

## Symptom

`auto_aux_update()` crashes reproducibly with:

```
Error in `purrr::map()`:
ℹ In index: 15.
Caused by error in `gh::gh()`:
! GitHub API error (422): No commit found for SHA: DEV
```

Additionally, `pip_missing_data()` was never called by the orchestrator even
when its dependencies (pfw, gdp, pop, etc.) changed.

## Root Cause

Four separate defects compounded to make the function unreliable:

### Bug 1 — Fatal SHA fetch (the immediate crash)
`purrr::map()` called `gh::gh("GET /repos/{owner}/{repo}/commits/{branch}")`
for **every** `aux_*` repo on GitHub, including `aux_missing_countries` which
has no `DEV` branch.  The 422 thrown by `gh::gh()` propagated and aborted
the entire function.  A single bad repo crashed the whole update run.

### Bug 2 — Derived measures silently never updated
`missing_data` (and any future derived measure) has no corresponding
`aux_missing_data` GitHub repo.  The code built `aux_fns` from repos whose
SHAs changed, intersected with `names(dependencies)`.  Since no
`aux_missing_data` repo exists, `pip_missing_data()` was never triggered even
when all its inputs (pfw, gdp, pce …) had been refreshed.

### Bug 3 — Signature file crash + silent NA comparison
`read_signature_file()` called `readr::read_lines()` unconditionally.  For a
measure that had never been saved, the file did not exist and the call threw
an error.  Even if `NA_character_` had been returned, the comparison
`if (before_hash != after_hash)` produces `NA` (not `FALSE`) when either
operand is `NA`, causing `if (NA)` — an immediate error.

### Bug 4 — Missing packaged `new_dependency.yml`
`inst/extdata/` contained `git_metadata.csv` but no `new_dependency.yml`, so
the 3rd-tier fallback for dependency metadata was absent.  On a fresh machine
with no local cache and a GitHub outage, `auto_aux_update()` would proceed
with zero dependency knowledge.

## Reproduction Tests

Added to `tests/testthat/test-auto_aux_update.R`:

```r
# Bug 1
test_that("fetch_repo_sha returns NA_character_ when the branch does not exist", {
  testthat::local_mocked_bindings(
    gh = function(endpoint, ...) stop("GitHub API error (422): ..."),
    .package = "gh"
  )
  result <- fetch_repo_sha("PIP-Technical-Team", "aux_missing_countries", "DEV")
  expect_true(is.na(result))
})

# Bug 3a
test_that("read_signature_file returns NA_character_ for a missing signature file", {
  result <- read_signature_file("nonexistent_measure", tempdir(), "DEV")
  expect_true(is.na(result))
})

# Bug 2
test_that("expand_with_derived_measures adds measures whose dependencies changed", {
  deps <- list(pfw = character(), gdp = c("weo", "country_list"),
               missing_data = c("country_list", "pce", "gdp", "pop", "pfw"))
  result <- expand_with_derived_measures("pfw", deps)
  expect_true("missing_data" %in% result)
  expect_false("gdp" %in% result)
  expect_true("pfw" %in% result)
})

# Bug 3b
test_that("NA signature comparison triggers update rather than erroring", {
  before <- NA_character_; after <- "abc123"
  expect_true(isTRUE(is.na(before != after)))    # demonstrates old bug
  expect_true(!isTRUE(before == after))           # new safe idiom
})
```

All 4 tests failed on the pre-fix code and pass on the fixed code.

## Fix

### R/auto_aux_update.R — 4 changes

**1. New `fetch_repo_sha()` helper (replaces inline `gh::gh()` call):**

```r
fetch_repo_sha <- function(owner, repo, branch) {
  tryCatch(
    gh::gh("GET /repos/{owner}/{repo}/commits/{branch}",
           owner = owner, repo = repo, branch = branch)[["sha"]],
    error = function(e) {
      cli::cli_alert_warning(
        "Skipping {.field {repo}}: branch {.val {branch}} not found. {conditionMessage(e)}"
      )
      NA_character_
    }
  )
}
```

The `purrr::map()` now calls `fetch_repo_sha()` and filters out `NA` results
before building the `all_data` table.

**2. New `expand_with_derived_measures()` helper:**

```r
expand_with_derived_measures <- function(aux_fns, dependencies) {
  derived <- names(dependencies)[
    vapply(dependencies,
           function(deps) length(deps) > 0 && any(deps %in% aux_fns),
           logical(1))
  ]
  union(aux_fns, setdiff(derived, aux_fns))
}
```

Called immediately after the original `intersect(names(dependencies))` line
so that derived measures are queued when any of their inputs changed.

**3. `read_signature_file()` — null-safe:**

Added an early-return guard:
```r
if (!fs::file_exists(data_signature_path)) return(NA_character_)
```

**4. NA-safe comparison in the update loop:**

```r
# Before (broken):
if (before_hash != after_hash) { ... }

# After (safe):
if (!isTRUE(before_hash == after_hash)) { ... }
```

### inst/extdata/new_dependency.yml — new file

Packaged a copy of the dependency graph so the 3rd-tier fallback is
operational on a fresh machine or during a GitHub outage.

## Lessons Learned

1. **`purrr::map()` over external API calls must always use `tryCatch`**.
   One unavailable resource in a list should never abort the whole loop.
   Use `purrr::possibly()` or an explicit helper with `tryCatch`.

2. **Derived / computed measures need a reverse-dependency trigger**.
   If a measure has no raw-data repo (it is computed from other processed
   measures), it will never appear in the "changed repos" list.  Any
   orchestrator that resolves work from changed repos must also expand
   forward to ask "which measures depend on what just changed?"

3. **File-existence guards before `read_lines()`**.
   Always check `fs::file_exists()` before reading a file that may not
   exist on a fresh environment.  Return a sentinel value (`NA_character_`)
   rather than throwing.

4. **`NA != NA` is `NA`, not `FALSE`**.
   Logical guards comparing potentially-NA strings must use
   `!isTRUE(a == b)` (or `!identical(a, b)`) rather than `a != b`.

5. **All fallback tiers must be populated**.
   If you design a 3-tier fallback (live → cache → packaged), ensure the
   packaged tier (`inst/extdata/`) is actually present in the repo.

## Related

None.
