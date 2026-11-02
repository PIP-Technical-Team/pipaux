---
date: 2026-10-30
title: "M2 Phase 2 Step 4: Save CPI, PPP, and PFW Through Stamp"
status: completed
completed-date: 2026-10-30
completed-phases: [1, 2]
---

# Plan: M2 Phase 2 Step 4 — Save CPI, PPP, and PFW Through Stamp

- **Date:** 2026-10-30
- **Status:** completed
- **Branch:** `feat/m2-aux-stamp-cpi-ppp-pfw` (from `PROD` at `27c5a3c8eab8ddcabb032d620af6a1f76e62f444`)
- **Deviation Policy:** `deviate: auto`
- **Umbrella Reference:** `E:/PovcalNet/01.personal/wb384996/PIP/pipsystem/.cg-docs/plans/2026-10-10-m2-walking-skeleton.md` (`1186b70`, Phase 2 Step 4, lines 290-303; Contracts E1-E3 lines 167-169, 185; `notes/pipaux.md` A1-A4)
- **Dependency Pins:** `stamp` 0.1.0 at `4a1fa5d` (`ready_cran`); `pipload` at `4ef0d90` (`feat/m2-stamp-adapter`)

## Context & Problem Statement

In `pipaux` (`PROD` at `27c5a3c`), the three M2 skeleton auxiliary series (`cpi`, `ppp`, `pfw`) have four contract gaps against Contracts E1-E3 and Step 4 (`save-cpi-ppp-and-pfw-through-stamp`):

1. **CPI persisted long key & post-melt validation (`R/aux_cpi.R:185-239, 438-518`):** `cpi_validate_output()` runs only on the wide pre-melt table (`country_code, year, survey_acronym, reporting_level`) before `melt()`, never on the persisted long table (`country_code, year, survey_acronym, reporting_level, cpi_year`), and `key_cols` in `aux_cpi_update()` is ordered `c("country_code", "cpi_year", "reporting_level", "year", "survey_acronym")` instead of Contract E1 (`country_code, year, survey_acronym, reporting_level, cpi_year`). Also `1:length(num_var_list)` at `R/aux_cpi.R:499` fails when `num_var_list` is empty.
2. **PPP key order & deterministic default flags (`R/aux_ppp.R:113-130, 241-272`):** `aux_ppp_update()` sets `key_cols` with `adaptation_version` before `release_version` and orders rows only by `country_code, reporting_level, ppp_year`, whereas Contract E2 and `ppp_validate_output()` (`R/aux_ppp.R:339-344`) specify `country_code, reporting_level, ppp_year, release_version, adaptation_version`.
3. **PFW key drift (`R/aux_pfw.R:215-227, 636-639`; `tests/testthat/test-pfw.R:5, 71, 237-244`):** `pfw_validate_output()` checks uniqueness on `(country_code, year, welfare_type, is_alt_welf)` while `aux_pfw_update()` saves `pk = c("country_code", "surveyid_year", "welfare_type")`, omitting `is_alt_welf` generated for alternative-welfare rows in `aux_pfw_clean()` (`R/aux_pfw.R:113-138`). Moreover, `make_pfw_raw()` and `make_pfw_output()` in `test-pfw.R` use invalid region codes (`"Sub-Saharan Africa"`, `"SSF"`) and omit `is_alt_welf`, masking key validation failures.
4. **Stamp persistence & content-based versioning (`R/utils.R:470-488`):** `pip_aux_save()` discards the result of `pipload::pip_write()` and returns `invisible(TRUE)` instead of a save receipt/pin (`path`, `version_id`, `skipped`, `reason`). In addition, `data.table` objects flowing into `pip_aux_save()` carry volatile `attr(x, "gh")` metadata, which `stamp::st_hash_obj()` includes in `content_hash`, causing a data-neutral raw source commit (changed `gh_raw_sha`) to create an unnecessary `stamp` artifact version.

## Scope & Rules

- **In Scope:** Only `cpi`, `ppp`, and `pfw` in `R/aux_cpi.R`, `R/aux_ppp.R`, `R/aux_pfw.R`, `R/utils.R`, `man/aux_cpi.Rd`, `man/aux_ppp.Rd`, `man/aux_pfw.Rd`, `man/cpi_validate_output.Rd`, `man/pip_aux_save.Rd`, `tests/testthat/test-cpi.R`, `tests/testthat/test-ppp.R`, `tests/testthat/test-pfw.R`, `tests/testthat/test-utils.R`, and a focused integration test file `tests/testthat/test-m2-stamp-cpi-ppp-pfw.R`.
- **Out of Scope:** Other auxiliary measures (`gdp`, `pop`, `pce`, `gdm`, `wdi`, etc.), `Y:\` filesystem writes, and live GitHub API calls.
- **Storage Rule:** Every temporary directory, R session `TMP`/`TEMP`/`TMPDIR`, test library, and `stamp` root MUST reside on `E:\` (`E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp`). Never write to `C:\` or `Y:\`.
- **Minimal First:** Build the smallest end-to-end solution using installed tools (`data.table`, `collapse`, `testthat`, `stamp`, `pipload`).

## Phase 1: Reconcile CPI, PPP, and PFW Schemas, Primary Keys, and Validators

**Goal:** Align cleaned output schemas, `key_cols`, function signatures, and `*_validate_output()` for `cpi`, `ppp`, and `pfw` with Contracts E1-E3 and verify with unit tests.

### Steps

- [x] **Step 1.1 — CPI persisted long key and validator (`R/aux_cpi.R`, `tests/testthat/test-cpi.R`):**
  - Update `cpi_validate_output()` (`R/aux_cpi.R:454-561`) so it detects the persisted long schema (`all(c("cpi_year", "cpi_value") %in% names(cpi))` and wide `cpi2011`/`cpi2017` columns absent) vs the pre-melt wide schema:
    - For the persisted long schema, validate `country_code` (character, not NA), `year` (integer, not NA), `survey_acronym` (character, not NA), `reporting_level` (character in `c("national", "rural", "urban")`, not NA), `cpi_year` (integer, not NA), `survey_year`/`cpi_value`/`ccf` (numeric), and `is_uniq(country_code, year, survey_acronym, reporting_level, cpi_year)`.
    - For the wide schema, replace `1:length(num_var_list)` with `seq_along(num_var_list)` and keep existing checks intact.
  - Update signatures of `aux_cpi(..., verbose = FALSE, ...)` (`R/aux_cpi.R:18-23`) and `aux_cpi_update(..., verbose = FALSE, ...)` (`R/aux_cpi.R:146-152`) so `verbose` and `...` propagate cleanly to `pip_aux_save()`.
  - In `aux_cpi_update()`, set `key_cols <- c("country_code", "year", "survey_acronym", "reporting_level", "cpi_year")`, call `cpi_validate_output(cpi, detail = detail)` on the melted long table before `pip_aux_save()`, set `setattr(cpi, "aux_name", "cpi")`, `setattr(cpi, "aux_key", key_cols)`, `setattr(cpi, "gh", gh)`, and `data.table::setkeyv(cpi, key_cols)`.
  - In `tests/testthat/test-cpi.R`, add tests for persisted long CPI validation (valid long fixture passes; duplicate `(country_code, year, survey_acronym, reporting_level, cpi_year)` fails; NA in `cpi_year` or non-numeric `cpi_value` fails).
- [x] **Step 1.2 — PPP key order and deterministic default flags (`R/aux_ppp.R`, `tests/testthat/test-ppp.R`):**
  - In `aux_ppp_clean()` (`R/aux_ppp.R:53-56, 120-137`), fall back to `2017` if `default_year` is `NULL`, and compute `d1 := release_version == max(release_version)` and `d2 := d1 & (adaptation_version == max(adaptation_version[d1]))` by `.(code, ppp_year)` so `ppp_default` and `ppp_default_by_year` are deterministic booleans without `NA` intermediates.
  - Update signatures of `aux_ppp(..., verbose = FALSE, ...)` (`R/aux_ppp.R:16-22`) and `aux_ppp_update(..., verbose = FALSE, ...)` (`R/aux_ppp.R:171-177`).
  - In `aux_ppp_update()` (`R/aux_ppp.R:244-314`), guard `if (identical(branch, "main"))` after loading, set `key_cols <- c("country_code", "reporting_level", "ppp_year", "release_version", "adaptation_version")` (Contract E2), set `data.table::setkeyv(ppp, key_cols)` and `data.table::setkeyv(ppp_vintage, key_cols_vintage)`, pass `verbose = verbose, ...` to `pip_aux_save(x = ppp, ...)`, and pass `verbose = verbose` plus any explicit `alias` from `list(...)` (routing-only, without leaking `ppp` metadata/parents) to `pip_aux_save(x = ppp_vintage, id = "ppp_vintage", pk = key_cols_vintage, ...)`.
  - In `tests/testthat/test-ppp.R`, add a test verifying that adding a newer PPP vintage column (`ppp_2017_v2_v1`) deterministically shifts `ppp_default` and `ppp_default_by_year` from `v1_v2` to `v2_v1` for the affected country/coverage/year.
- [x] **Step 1.3 — PFW key reconciliation (`R/aux_pfw.R`, `tests/testthat/test-pfw.R`):**
  - Update signature of `aux_pfw_update(..., verbose = FALSE, ...)` (`R/aux_pfw.R:177-183`) and forward `verbose = verbose, ...` from `aux_pfw()` (`R/aux_pfw.R:52-59`) to `aux_pfw_update()` and `pip_aux_save()`.
  - Reconcile the PFW primary key to `c("country_code", "surveyid_year", "welfare_type", "is_alt_welf")` (Contract E3) in both `pfw_validate_output()` (`R/aux_pfw.R:641-648`) and `aux_pfw_update()` (`R/aux_pfw.R:212-235`):
    - In `pfw_validate_output()`, validate `is.logical` on `is_alt_welf`, `not_na` on `country_code, year, surveyid_year, welfare_type, is_alt_welf`, and `is_uniq(country_code, surveyid_year, welfare_type, is_alt_welf)`.
    - In `aux_pfw_update()`, guard `if (identical(branch, "main"))` after loading, set `key_cols <- c("country_code", "surveyid_year", "welfare_type", "is_alt_welf")`, set `setattr(pfw, "aux_key", key_cols)`, and call `data.table::setkeyv(pfw, key_cols)` before `pip_aux_save(..., verbose = verbose)`.
  - In `tests/testthat/test-pfw.R`, fix `make_pfw_raw()` (`region = c("SSA", "EAP")`) and `make_pfw_output()` (`region_code = c("SSA", "EAP")`, `surveyid_year = c(2010L, 2011L)`, `is_alt_welf = c(FALSE, FALSE)`), add positive baseline validation tests (`expect_no_error` on valid raw and output fixtures), and test duplicate key rejection on `(country_code, surveyid_year, welfare_type, is_alt_welf)` as well as preservation of both main (`is_alt_welf = FALSE`) and alternative welfare (`is_alt_welf = TRUE`) rows in `aux_pfw_clean()`.

**Phase 1 Check:** Run `test-cpi.R`, `test-ppp.R`, and `test-pfw.R` via `Rscript` with `TMP`/`TEMP`/`TMPDIR` on `E:\`; all unit tests pass with 0 failures.

## Phase 2: Wire `pip_aux_save()` Through `pipload` / `stamp` and Verify L-aux (V7)

**Goal:** Persist validated `cpi`, `ppp`, and `pfw` through `pip_aux_save()` -> `pipload::pip_write()` -> `stamp::st_save()` with content-based versioning, metadata pins, and retained `version_id` resolution on skipped saves.

### Steps

- [x] **Step 2.1 — Enhance `pip_aux_save()` in `R/utils.R` (`R/utils.R:458-564`, `tests/testthat/test-utils.R`):**
  - Caller compatibility check (all 24 `R/aux_*.R` callers plus `R/simulate_changes.R` and `R/update_aux_data.R` verified): callers either ignore the return value or do `saved <- pip_aux_save(...); return(invisible(saved))`, and non-tabular callers (`aux_cp`, `aux_censoring`) pass a `list` without `pk`.
  - Update `pip_aux_save(x, id, verbose = TRUE, ...)`:
    - Capture `dots <- list(...)`; resolve `alias <- dots$alias %||% get_from_auxenv("aux_alias")` (removing `alias` from `dots` before forwarding).
    - Extract `gh_attr <- attr(x, "gh", exact = TRUE)`. Ensure `dots$metadata <- dots$metadata %||% list()`; if `!is.null(gh_attr)` and `is.null(dots$metadata$gh)`, set `dots$metadata$gh <- gh_attr`.
    - Prepare `x_to_save`: for `data.frame` / `data.table` inputs, make a shallow copy (`data.table::copy(x)` if `data.table::is.data.table(x)`) so the caller's `x` retains `attributes(x)$gh`, strip volatile `gh` attribute (`attr(x_to_save, "gh") <- NULL`) while preserving `aux_name` and `aux_key` (setting defaults from `id` and `dots$pk` when absent), and when `data.table::is.data.table(x_to_save)` with `!is.null(dots$pk)` and `all(dots$pk %in% names(x_to_save))`, call `data.table::setkeyv(x_to_save, dots$pk)`. Non-tabular (`list`) inputs simply strip `attr(x_to_save, "gh")` if present.
    - Delegate via `res <- do.call(pipload::pip_write, c(list(x = x_to_save, id = id, alias = alias, verbose = verbose), dots))`.
    - Normalize `res$skipped <- isTRUE(res$skipped)`. When `res$skipped` is `TRUE` and `res$version_id` is absent/empty, resolve `res$version_id` from `stamp::st_versions(res$path %||% id, alias = alias)$version_id[1L]`.
    - Validate that `res$version_id` is a non-missing, non-empty character scalar; otherwise abort via `cli::cli_abort()`. Return `invisible(res)`.
  - In `tests/testthat/test-utils.R`, replace the skipped `pip_aux_save()` placeholder with tests against an isolated `E:\` `stamp` root covering `data.table` (with `pk` and `gh` attribute), `list` (non-tabular compatibility), skipped re-saves, and the `stamp::st_versions()` fallback.
- [x] **Step 2.2 — Focused L-aux / V7 integration test suite (`tests/testthat/test-m2-stamp-cpi-ppp-pfw.R`):**
  - Create an isolated temporary `stamp` root under `E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp` (cleaning up on exit, restoring `.pipaux` and `stamp` alias/options state, never touching `Y:\`, `C:\`, or GitHub network calls):
    - Initialize the temporary `stamp` root (`stamp::st_init(tmp_dir, alias = test_alias, verbose = FALSE)`), set `.pipaux$aux_alias <- test_alias` and `.pipaux$aux_data_path <- tmp_dir`, mock `pipfun::get_wrk_release` and `pipfun::get_pip_folders` (via `testthat::local_mocked_bindings(.package = "pipfun")`) to return `tmp_dir` for `"aux_data"`, and persist a minimal `country_list` artifact (`country_code = c("AAA", "BBB")`, `region_code = c("SSA", "EAP")`) into `tmp_dir` so the real `pipload::load_aux_data()` resolves `country_list`, `cpi`, `ppp`, and `pfw` from `tmp_dir`.
    - Verify against pinned `stamp` 0.1.0 (`4a1fa5d`) and `pipload` (`4ef0d90`):
      1. **Round-trip persistence & metadata pins:** Valid `cpi`, `ppp`, and `pfw` fixtures (both direct `pip_aux_save()` and mocked-GitHub `aux_cpi_update()`, `aux_ppp_update()`, `aux_pfw_update()`) save and read back via `pipload::pip_read()` and `pipload::load_aux_data()` as `data.table` with exact keys (`stamp::st_get_pk()` and `data.table::key()`), column classes, and sidecar metadata (`gh`, `code_hash`, `code_label`, `pk$keys`).
      2. **Duplicate-key rejection before save:** Duplicate keys on persisted `cpi` (`country_code, year, survey_acronym, reporting_level, cpi_year`), `ppp` (`country_code, reporting_level, ppp_year, release_version, adaptation_version`), and `pfw` (`country_code, surveyid_year, welfare_type, is_alt_welf`) fail before writing any new artifact or version.
      3. **Content-based versioning & retained `version_id` on skip:** Re-saving unchanged cleaned `cpi`, `ppp`, and `pfw` — including when `gh$gh_raw_sha` changes to simulate a data-neutral raw source commit — creates no new `stamp` version (`nrow(stamp::st_versions(...)) == 1L`, `res$skipped == TRUE`) and returns the exact retained `version_id` pin; modifying one `cpi_value` creates a second `stamp` version (`nrow == 2L`, `res2$skipped == FALSE`, `res2$version_id != res1$version_id`), and a subsequent unchanged save returns the retained `v2` `version_id`.

**Phase 2 Check (L-aux / V7):** Run `test-cpi.R`, `test-ppp.R`, `test-pfw.R`, `test-utils.R`, and `test-m2-stamp-cpi-ppp-pfw.R` via `Rscript` with `stamp` `4a1fa5d` and `pipload` `4ef0d90` on `E:\`; all assertions pass.

## Risks & Mitigations

1. **Volatile `attr(x, "gh")` changing `stamp::st_hash_obj(x)`:** `as.data.frame.data.table()` preserves custom attributes on `x`, so `st_hash_obj()` hashes `attr(x, "gh")` unless stripped from the copy passed to `pipload::pip_write()`. *Mitigation:* `pip_aux_save()` copies `gh` to `metadata$gh` (stored in `sidecar$gh`) and removes `gh` from the shallow copy passed to `pipload::pip_write()`, keeping `aux_name`, `aux_key`, and the caller's in-memory `x` unchanged.
2. **Global R library version drift (`stamp 0.0.11` vs pinned `0.1.0` at `4a1fa5d`):** The shared `win-library` has `stamp 0.0.11`. *Mitigation:* Install pinned `stamp` (`4a1fa5d`) and `pipload` (`4ef0d90`) into `E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp/lib` for verification AND keep a fallback lookup via `stamp::st_versions()` in `pip_aux_save()` so skipped saves return `version_id` under both versions.
3. **Isolating `pipload::load_aux_data()` from `Y:\`:** `load_aux_data()` queries `pipfun::get_pip_folders("aux_data")` and `stamp::st_alias_list()`. *Mitigation:* Mock `pipfun::get_wrk_release` and `pipfun::get_pip_folders` to the temporary `E:\` `stamp` root and seed a real `country_list` artifact there so `load_aux_data()` runs unmocked end-to-end.

## Plan Review Log (`/cg-plan-review`)

- **[P2.1]** Fixed in Steps 1.1–1.3: Explicitly updated signatures of `aux_cpi`/`aux_cpi_update`, `aux_ppp`/`aux_ppp_update`, and `aux_pfw`/`aux_pfw_update` to accept and forward `verbose = FALSE, ...`, and routed `ppp_vintage` with only `verbose` and explicit `alias` override.
- **[P2.2]** Fixed in Step 2.2 & Risk 3: Added scoped `pipfun::get_wrk_release` and `pipfun::get_pip_folders` mocks plus a seeded `country_list` artifact in the temporary `E:\` `stamp` root so `pipload::load_aux_data()` executes unmocked against `E:\`.
- **[P2.3]** Fixed in Step 2.1: Verified all `pip_aux_save()` callers, preserved non-tabular (`list`) compatibility, and added `test-utils.R` tests for both `data.table` and `list` callers.

## Later (Deferred Safeguards per Minimal First)

- Updating `sidecar$gh` in place on skipped saves without creating a new `stamp` snapshot (only relevant if `check_y_drive_status()` is used instead of `stamp` lineage in a future milestone).
- Expanding `pip_aux_save()` key/schema reconciliation to non-skeleton auxiliary measures (`gdp`, `pop`, `pce`, `gdm`, `wdi`).
