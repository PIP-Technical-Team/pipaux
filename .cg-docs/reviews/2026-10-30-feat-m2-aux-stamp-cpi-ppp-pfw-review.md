# Code Review Report: `feat/m2-aux-stamp-cpi-ppp-pfw`

- **Date:** 2026-10-30
- **Branch:** `feat/m2-aux-stamp-cpi-ppp-pfw`
- **Base:** `PROD` (`27c5a3c8eab8ddcabb032d620af6a1f76e62f444`)
- **Plan:** `.cg-docs/plans/2026-10-30-m2-aux-stamp-cpi-ppp-pfw.md`
- **Mode:** `review:auto` -> `/cg-fix-triage P0 P1 P2 P3` -> `mode:verify`
- **Final Verdict:** **PASS (0 unresolved findings across P0/P1/P2/P3)**

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
