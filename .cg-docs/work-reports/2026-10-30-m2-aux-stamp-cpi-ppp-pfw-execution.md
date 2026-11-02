# Execution Report: M2 Phase 2 Step 4 — Save CPI, PPP, and PFW Through Stamp

- **Plan:** `.cg-docs/plans/2026-10-30-m2-aux-stamp-cpi-ppp-pfw.md`
- **Review Report:** `.cg-docs/reviews/2026-10-30-feat-m2-aux-stamp-cpi-ppp-pfw-review.md`
- **Branch:** `feat/m2-aux-stamp-cpi-ppp-pfw`
- **Started:** 2026-10-30T16:40:00Z
- **Completed:** 2026-10-30T17:28:00Z
- **Status:** completed
- **Options:** `review:auto`, `deviate:auto`

## Phase Progress

- [x] Phase 1: Reconcile CPI, PPP, and PFW Schemas, Primary Keys, and Validators
- [x] Phase 2: Wire `pip_aux_save()` Through `pipload` / `stamp` and Verify L-aux (V7)

## Evidence Log

- **Phase 1 & Phase 2 Test Suite (post-triage):** `Rscript --vanilla` with `stamp` 0.1.0 (`4a1fa5d`) and `pipload` (`4ef0d90`) on `E:\`:
  - `tests/testthat/test-cpi.R`: 22 PASS, 0 FAIL, 0 WARN
  - `tests/testthat/test-ppp.R`: 25 PASS, 0 FAIL, 0 WARN
  - `tests/testthat/test-pfw.R`: 33 PASS, 0 FAIL, 0 WARN
  - `tests/testthat/test-utils.R`: 50 PASS, 0 FAIL, 0 WARN
  - `tests/testthat/test-m2-stamp-cpi-ppp-pfw.R`: 76 PASS, 0 FAIL, 0 WARN
- **Review & Verification:** 9 review agents dispatched (`review:auto`), all P2 and P3 findings fixed via `/cg-fix-triage P0 P1 P2 P3`, and confirmed `[RESOLVED]` via `/cg-review mode:verify`.

## Deviations & Decisions

- Set `PIP_ROOT_DIR` to an empty stub folder on `E:\` during test runs so `R/zzz.R`'s top-level `pipfun::pip_create_globals()` call never touches `//w1wbgencifs01/pip/` or `Y:\`, then cleaned up `.tmp` on completion.

## Commit and Merge Readiness Recheck (2026-11-02)

- Implementation was already committed and pushed as `798242c91f661d259bc2c45cce032b6a93bb1d6b` (`feat(aux): prepare M2 auxiliary artifacts`) when this follow-up began. No implementation changes were needed for the recheck.
- Reinstalled the pinned local sources (`stamp` `4a1fa5d`, version `0.1.0`; `pipload` `4ef0d90`) into `E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp/m2-verify-20261102/lib`.
- Reran the five focused test files in a fresh R 4.5.2 process with `TMP`, `TEMP`, `TMPDIR`, `HOME`, `R_USER`, caches, and the stub `PIP_ROOT_DIR` on `E:/`. Installed binary dependencies on `C:/` were read only. Tests used mocked GitHub input and isolated stamp storage, with no shared-drive access or live GitHub data requests.

| Test file | Passed | Failed | Warnings | Skipped |
|---|---:|---:|---:|---:|
| `test-cpi.R` | 22 | 0 | 0 | 2 |
| `test-ppp.R` | 25 | 0 | 0 | 1 |
| `test-pfw.R` | 33 | 0 | 0 | 1 |
| `test-utils.R` | 50 | 0 | 0 | 1 |
| `test-m2-stamp-cpi-ppp-pfw.R` | 88 | 0 | 0 | 0 |
| **Total** | **218** | **0** | **0** | **5** |

- The five skips are existing placeholders for external-service/filesystem integration; the focused L-aux/V7 file has no skips. The current measured L-aux/V7 count is 88, superseding the earlier reported count of 76.
- A second run with the default installed runtime (`stamp` `0.0.10`, `pipload` `1.0.0`) produced **215 passes and 3 failures**, with no errors/warnings and the same five skips. All three failures are missing restored `data.table` keys for CPI, PPP, and PFW at `test-m2-stamp-cpi-ppp-pfw.R:260,262,264`. This is a runtime compatibility blocker to unconditional production promotion; it is not a failure of the pinned M2 verification.
- GitHub confirms the default branch is `PROD`; there is no `main` branch. The feature implementation is directly ahead of `PROD` at `27c5a3c`, with no base divergence. No pull request or CI check runs exist for the implementation commit; an empty aggregate status is not a passing check.
- `DESCRIPTION` still selects `pipload@PROD`, while tested adapter commit `4ef0d90` remains one commit ahead of `pipload/PROD`. Tested stamp commit `4a1fa5d` remains on `ready_cran`, 27 commits ahead of `master`. Production dependency rollout/selection remains pending; neither dependency nor any production system was changed by this follow-up.
- Remaining umbrella M2 work includes real integration evidence and Phase 3 Steps 7-10 (deflation, mean, TEST release pin/readback, and integrated T1-T12 checks). Those downstream tasks are not additional implementation requirements for this pipaux step. No merge, PR creation, production update, or shared-drive operation was authorized or performed.

## Authorized Dependency and P3 Correction (2026-11-02)

- `DESCRIPTION` now requires stamp >= 0.1.0 and pipload >= 1.0.0 and selects the full tested commits `4a1fa5d92313aa223296fae0e29cf3446a314aea` and `4ef0d90a974cb6ff0725da69373bde421dd04ad5` through `Remotes`. This removes the assumption that unmerged upstream default branches supply those fixes. Normal dependency-aware installation must honor the remotes; no existing shared/production library was upgraded.
- Added explicit test-only `callr` and `pkgload` dependencies, both already installed on this machine. The two storage-test files now run in disposable child R processes with the caller's library paths and E-drive startup temp/cache paths. Private stamp registry/state/options environment snapshots and mutations have been removed. Public snapshots verify that caller aliases/default/options are unchanged. Existing read-only `stamp:::st_hash_code()` assertions are separate and intentionally unchanged.
- Installed-package verification found an additional real failure: `data.validator` evaluates unqualified `is_uniq()` expressions outside the installed package's import environment. Qualified all eight occurrences in the CPI/PPP/PFW raw/output validators as `assertr::is_uniq()`, leaving other measures and key contracts unchanged. Source-loaded testing had hidden this resolution problem.
- Final focused source run passes all **218 original assertions** (22 CPI + 25 PPP + 33 PFW + 50 utility child + 88 L-aux child), plus **4 new parent wrapper/state assertions**, with no failures/errors/warnings and the same five existing external-service skips. Parent testthat totals report 84 assertions; child totals (50 + 88) are printed separately, not silently lost or double-counted.
- The full five-file focused suite also passes against an installed pipaux package in the isolated E-drive library, using installed-package namespace resolution rather than `pkgload`.
- Both subprocess files pass two consecutive directory-discovery runs in one caller R session with no alias conflicts or marker leakage. An intentional child exception was correctly propagated while preserving a non-default caller alias/options configuration. The deliberate negative experiment is not an unresolved suite failure.
- An incompatible default stamp 0.0.10 is now rejected by the dependency requirement (`The package "stamp" (>= 0.1.0) is required.`), before running the pipeline. Full-SHA declarations were checked against the local source HEADs.
- Independent architecture/reproducibility and testing/code-quality reviews found no actionable findings in the dependency/isolation changes. Final `/cg-review mode:verify` (`ses_e631b8ec7ffe6ln9Z056JW5gtV`) confirmed dependency alignment, private-state P3 and installed-validator correction `[RESOLVED]`, with no new actionable P0-P3 findings. That review verified code/evidence consistency; the runtime checks were executed by the main agent.
- All agent-created outputs are under `E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp/m2-align-20261102`; installed dependencies on C: were read only. No live GitHub raw-data requests, Y-drive operations, production installs or PROD merge were performed.

## Pkgdown Dependency Correction (2026-11-02)

- PR 119's first pkgdown run (`41506873211`) failed before the site build, in pak lockfile resolution. The same full root set and dependency coverage reproduced the error locally with installed pak 0.11.1; no new tooling dependency was added.
- Explicit solver diagnostics isolate the conflict: pipfun PROD requests unpinned `randrescastaneda/stamp` (default version 0.0.11), while pipaux requires the immutable stamp 0.1.0 source. Adding direct workflow roots does not resolve it. Recursive optional pipdata dependencies are not the cause of the original solve.
- With explicit user approval, created and pushed pipfun branch `fix/m2-stamp-pin` from PROD `0c6a78d`. Commit `aa6dfe888600820a525b43419268d531b6f2fb82` changes only its DESCRIPTION stamp remote to the same full SHA used here. pipfun PROD was neither updated nor merged, and the local pipfun checkout was restored to PROD.
- pipaux now pins pipfun to that supporting immutable commit. The original full pak solve succeeds for 126 packages, selecting stamp 0.1.0, pipload 1.0.0 and pipfun 1.0.1. No workflow, dependency type, version minimum or build check was removed or weakened.
- Exported the exact supporting commit into an E-drive source archive, installed it with the existing stamp/pipload pins in an isolated E library, verified the installed pipfun remote metadata, and reran all five focused files: 222 assertions pass (84 parent plus 138 child), zero test failures/errors/warnings, five existing external-service skips. The archive extractor's pax-header warning is packaging-only, not a failing test.
- Independent metadata review (`ses_e6232defcffeG10vqttcQg80ZF`) found no P0-P3 issues. It inspected the supporting published patch and main pins; runtime execution was performed by the main agent.
- The user authorized a pipaux PROD merge only after corrected exact-head CI passes; that outcome is tracked in orchestration run `20261102T191337Z-pipaux`, not presumed by this pre-push record.

### Documentation Root Correction

- GitHub run `41514056928` at `6f026fe` got past the dependency solve, then failed package lazy loading: `[ENOENT] Failed to search directory '/pip_ingestion_pipeline/pc_data/cache/clean_survey_data'`. `R/zzz.R` initializes legacy globals at load time, so the CI runner needs a cache-directory fixture even though the site needs no survey data.
- Added job-scoped `PIP_ROOT_DIR` under `runner.temp` and a pre-installation step creating one empty `20200101_2017_01_01_TEST` directory in that documentation root. A nonempty directory listing avoids the existing `max(empty)` initialization warning; the fixture contains no survey files or numerical outputs.
- Existing dependency setup arguments, site-build command, and non-PR deployment guard are unchanged. No package runtime functions, checks, examples or tests were disabled.
- Local YAML parsing/step-order validation and package initialization with the analogous isolated E-drive fixture pass with warnings promoted to errors. Reviewer `ses_e6232defcffeG10vqttcQg80ZF` reports no P0-P3 findings in the workflow addition. Final exact-head CI is still required before merging.
