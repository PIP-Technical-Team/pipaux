# pipaux — Project Charter

## Objective

`pipaux` is an R package that orchestrates the auxiliary data pipeline for the
World Bank's Poverty and Inequality Platform (PIP). It manages two main
processes:

1. **Update auxiliary data** — efficiently syncs raw data from GitHub
   auxiliary repositories to the PIP Y-drive release system, with
   change-detection to avoid unnecessary writes and a dependency-aware
   update cascade.
2. **Compare auxiliary data** — identifies value changes, row additions,
   and row removals across releases or vintages.

## Constraints

- Requires access to the PIP Y-drive (`Y:\PIP_ingestion_pipeline_v2\`) and a
  valid GitHub PAT (stored as `GITHUB_TOKEN` or equivalent).
- All Y-drive writes go through `pip_aux_save()` / `pipload::pip_write()` —
  never write raw files directly.
- Raw data is always fetched via `pipfun::load_from_gh()`. Preserve the
  returned `attributes(x)$gh` on every object that flows into
  `pip_aux_save()`.
- The `.pipaux` runtime environment (populated by
  `pipfun::setup_working_release()`) must be initialized before any update or
  load operation.
- Errors must be thrown explicitly (`cli::cli_abort()`, `stop()`). No silent
  fallbacks or `tryCatch()` swallowing errors without re-throwing.
- Branch naming convention: `paste0(release, "_", identity)` (e.g.
  `"20250101_PROD"`).
- Conventional commits required: `type(scope): description`.

## Architecture

| Layer | Key files |
|---|---|
| Measure-specific updaters | `R/aux_*.R` |
| Orchestration & dependency graph | `R/update_aux_data.R` |
| Status checks (GH + Y-drive) | `R/check_status.R` |
| Change comparison | `R/identify_changes.R` |
| Runtime environment helpers | `R/zzz.R`, `R/utils.R` |
| Interactive diagnostics | `dev/` (excluded from package) |

## Current Focus

- Maintain and extend per-measure `aux_*` functions.
- Keep `dev/_project_notes.md` updated as the single source of truth for
  onboarding and team knowledge.
- Ensure `check_github_status()` and `check_y_drive_status()` remain the
  authoritative decision points for all publish/skip logic.

## Dependencies (external R packages)

Core runtime: `pipfun`, `pipload`, `stamp`, `data.table`, `dplyr`, `purrr`,
`fst`, `qs`, `fs`, `cli`, `rlang`, `glue`, `digest`, `joyn`, `collapse`,
`data.validator`, `assertr`.

Dev/test only: `testthat`, `dm`, `myrror`, `gh`, `httr`, `wbstats`.
