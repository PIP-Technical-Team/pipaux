# Code Review Report: `feat/m2-aux-stamp-cpi-ppp-pfw`

- **Date:** 2026-10-30
- **Branch:** `feat/m2-aux-stamp-cpi-ppp-pfw`
- **Base:** `PROD` (`27c5a3c8eab8ddcabb032d620af6a1f76e62f444`)
- **Plan:** `.cg-docs/plans/2026-10-30-m2-aux-stamp-cpi-ppp-pfw.md`
- **Mode:** `review:auto` -> `/cg-fix-triage P0 P1 P2 P3` -> `mode:verify`
- **Pinned M2 Verdict (2026-10-30):** **PASS for the verified corrective changes**. This verdict does not certify older stamp runtimes or production promotion; see the recheck below.

## Agents Dispatched

- `@cg-code-quality` (`ses_e722e325cffeSFgDLyw3j1UjQV`, verify `ses_e72275717ffejWO78opXQun4Iq`)
- `@cg-testing` (`ses_e722e2095ffek5x7Y0ikMsBPzF`, verify `ses_e722740d4ffejBlsp0blGK1BCx`)
- `@cg-documentation` (`ses_e722e1281ffes6V766GPgCXBlI`, verify `ses_e722740d4ffejBlsp0blGK1BCx`)
- `@cg-version-control` (`ses_e722e0547ffe1BbkjXSW6v6Bu2` + main session `git status` / `git diff --stat`)
- `@cg-reproducibility` (`ses_e722df60bffe0cYMv58UD4Pd1X`, verify `ses_e72275717ffejWO78opXQun4Iq`)
- `@cg-performance` (`ses_e722de7feffe5VhQJT1yelW0Id` — 0 findings)
- `@cg-architecture` (`ses_e722dd8c6ffe5Il9JVRI71kyRu`, verify `ses_e722740d4ffejBlsp0blGK1BCx`)
- `@cg-data-quality` (`ses_e722dc748ffejDx8leYGZm47RE`, verify `ses_e722740d4ffejBlsp0blGK1BCx`)
- `@cg-fix-problems` (`ses_e722db52cffewnV2GqnDLzo3NU`, verify `ses_e722740d4ffejBlsp0blGK1BCx`)

## Findings & Resolution Summary

- **[RESOLVED] [P2.1]** Preserve `branch = "main"` when calling `pipfun::load_from_gh()` in `R/aux_cpi.R`, `R/aux_ppp.R`, and `R/aux_pfw.R` (`test-m2-stamp-cpi-ppp-pfw.R:217-236` verifies `c("cpi:main", "ppp:main", "pfw:main")`).
- **[RESOLVED] [P2.2]** Preserve `aux_name` and `aux_key` attributes on tabular objects in `pip_aux_save()` (`R/utils.R:484-522`), stripping only volatile `gh` so `merger_aux()` compatibility is preserved alongside pure content hashing.
- **[RESOLVED] [P2.3]** Enforce non-`C:` temporary directory resolution in `tests/testthat/test-utils.R:50-61` and `tests/testthat/test-m2-stamp-cpi-ppp-pfw.R:4-19`.
- **[RESOLVED] [P2.4]** Check `fs::file_exists()`, `fs::dir_exists()`, and nested artifact paths after duplicate-key rejection in `tests/testthat/test-m2-stamp-cpi-ppp-pfw.R:331-358`.
- **[RESOLVED] [P2.5 / P3]** Updated roxygen blocks and `man/pip_aux_save.Rd`, `man/aux_cpi.Rd`, `man/aux_ppp.Rd`, `man/aux_pfw.Rd`, and `man/cpi_validate_output.Rd` to match the updated signatures, return contracts, and dual-schema CPI validator.
- **[RESOLVED] [P3.1]** Isolated `is_alt_welf` in `tests/testthat/test-pfw.R:255-263` with identical `country_code`, `surveyid_year`, and `welfare_type`.
- **[RESOLVED] [P3.2]** Added regression coverage for skipped save after CPI v2 creation (`tests/testthat/test-m2-stamp-cpi-ppp-pfw.R:420-440`) and for the `stamp::st_versions()` fallback when `version_id` is `NULL` (`tests/testthat/test-utils.R:103-114`).

## Merge Readiness Recheck (2026-11-02)

- Pinned `stamp` `0.1.0` (`4a1fa5d`) and `pipload` (`4ef0d90`): **218 passing assertions**, zero failures/warnings, five existing external-service skips across the five focused files.
- Default installed `stamp` `0.0.10` with `pipload` `1.0.0`: **3 failing assertions** because CPI/PPP/PFW `data.table` keys are not restored on read-back (`test-m2-stamp-cpi-ppp-pfw.R:260,262,264`). A production merge/promotion is conditional on adopting the verified runtime or providing equivalent compatibility evidence.
- `PROD`, not `main`, is the repository's default branch. The implementation has no divergence from the current `PROD` base. There is no pull request and no remote check run for the implementation commit. Full package checks and live shared-drive/GitHub integration were not run.
- The initial architecture review also identified P3 coupling to private `stamp` environments in the test-isolation setup. At this recheck it still remained in `test-utils.R` and `test-m2-stamp-cpi-ppp-pfw.R`; it had not been fixed by the earlier verification. The earlier blanket claim that every P3 finding was resolved was too broad. This historical limitation is corrected by the authorized follow-up below.

## Authorized Correction Verification (2026-11-02)

**Verdict: PASS for the scoped corrections, no outstanding actionable P0-P3 findings.**

- **[RESOLVED] Dependency alignment:** `DESCRIPTION` now declares stamp >= 0.1.0 and pipload >= 1.0.0 and selects the full tested GitHub SHAs for stamp `4a1fa5d` and pipload `4ef0d90`. Incompatible stamp 0.0.10 is rejected before package loading; normal dependency-aware installation must honor the declared remotes. No shared or production package library was changed.
- **[RESOLVED] Private-state P3:** the utility and L-aux test files run through `tests/testthat/helper-stamp-process.R` in disposable R processes. No private stamp environment snapshots or mutations remain. Public caller-state snapshots remain identical. Existing read-only `stamp:::st_hash_code()` assertions are intentionally unchanged and are not claims of complete removal of private API references.
- **[RESOLVED] Installed-package validator failure:** installed testing exposed `data.validator` failing to resolve bare `is_uniq()` expressions. All eight occurrences in the three skeleton raw/output validators are now `assertr::is_uniq()` with keys and logic unchanged.
- Source and installed focused suites execute all 218 original assertions plus 4 new parent wrapper/state assertions (84 parent, 138 child), with zero failures/errors/warnings and five existing external-service skips. Child assertion counts are printed separately. Two repeated discovery runs and an intentional child-error experiment demonstrate process isolation and failure propagation.
- Reviews: architecture/reproducibility (`ses_e631b8ec8ffemkxWD52CaGTdnD`) and testing/code quality (`ses_e631b8ec7ffe6ln9Z056JW5gtV`) found no actionable issues. Final `/cg-review mode:verify` with the testing/code-quality reviewer confirmed all three corrections `[RESOLVED]` and reviewed code/evidence consistency. Runtime execution was performed by the main agent, not the reviewers.
- Remaining release-stage work is separate: a PR/merge approval for `PROD`, installation of the declared dependencies in the intended runtime, and any required live-data/full-package/CI evidence. None was represented as completed by these focused checks, and no PROD merge was performed.

## Pkgdown Fix Review (2026-11-02)

- The later user approval authorizes PR 119's merge only after successful checks, plus a metadata-only supporting pipfun branch/push (not a pipfun PROD merge).
- Supporting pipfun commit `aa6dfe888600820a525b43419268d531b6f2fb82` changes one DESCRIPTION remote to stamp `4a1fa5d`, removing the conflict with the older default stamp source. The only functional pipaux change in this correction is selecting that pipfun SHA in DESCRIPTION; stamp and pipload pins remain unchanged.
- Reviewer `ses_e6232defcffeG10vqttcQg80ZF` confirmed the metadata correction is consistent, with no P0-P3 findings. The same full pak dependency solve now succeeds (126 packages), and the focused 222 assertions pass with the exact supporting source installed.
- No checks, dependency coverage, workflow steps or version bounds were weakened. Exact corrected-head GitHub CI is still required before the conditional merge; this review does not substitute for that gate.
- The subsequent run passed dependency resolution but exposed missing documentation-root initialization. A separate reviewed workflow addition sets `PIP_ROOT_DIR` to a runner-temporary directory and creates one empty TEST-vintage cache directory before package installation. No source function or existing check changes.
- Reviewer `ses_e6232defcffeG10vqttcQg80ZF` inspected that initialization path and confirmed no P0-P3 findings. Local YAML/order and warning-free namespace initialization checks pass. Final GitHub CI, not this static review, remains the merge gate.
- **[RESOLVED] P2, Actions context placement:** GitHub rejected `runner.temp` in job-level `env` at head `47a3292`. The earlier YAML/static review missed this Actions schema restriction. The preparation step now uses shell `$RUNNER_TEMP` and writes the path through `$GITHUB_ENV`; actual shell execution and warning-free package loading pass locally. The same reviewer verified this correction with no new P0-P3 findings. An empty check collection or `CLEAN` merge status does not satisfy the successful exact-head CI gate.
