---
project-name: "pipaux"
team: "DECDG / GPID — World Bank"
last-reviewed: "2026-04-15"
---

# Compound GPID — Project Charter

## Objective

pipaux is an R package that manages the complete lifecycle of ~25 auxiliary
datasets for the Poverty and Inequality Platform (PIP) at the World Bank. It
downloads raw data from GitHub repositories and external sources (WDI API, WEO,
Maddison), cleans and transforms it, and saves it in a hash-signed, versioned
(vintage) format on a shared network drive. It also provides a simple API to
load that data into memory.

### Architecture Summary

**Function naming convention.** Each measure follows a 3-function pattern:
`pip_<measure>(action)` (public dispatcher), `pip_<measure>_update()` (ETL),
and optionally `pip_<measure>_clean()`.

**Auxiliary measures** (~25): cpi, ppp, gdp, pce, weo, wdi, maddison, sna,
nan (nowcast), pop, country_list, countries, regions, metaregion,
income_groups, indicators, dictionary, npl, pfw, gdm, metadata, censoring,
missing_data, cp (country profiles), pl, sub.

**Data storage.** Files are saved at
`{PIP_DATA_DIR}/_aux/{branch}/{measure}/{measure}.{qs,fst,dta}` with a
`_datasignature.txt` hash and a `_vintage/` directory for timestamped copies.
Loading priority: `.qs` → `.fst` → `.rds`.

**Hash-based idempotency.** Every update computes an xxhash64 digest. If the
hash matches the existing signature, no write occurs. `force = TRUE` bypasses
this check.

**`auto_aux_update()` — the production orchestrator.** Performs incremental
updates by: (1) reading `git_metadata.csv` (last-known commit SHAs) and
`new_dependency.yml` (dependency graph) from the `metadata` branch, (2)
comparing current HEAD SHAs of each `aux_*` repo to detect changes, (3)
running `pip_*` functions for changed measures in dependency order, and (4)
pushing the updated `git_metadata.csv` back to GitHub. Both metadata files
use a 3-tier fallback: live GitHub → local cache → packaged `inst/extdata`.

**DEV / PROD branching.** The `branch` parameter controls which GitHub branch
to pull raw data from and which `_aux/` subdirectory stores processed data,
enabling parallel development and production pipelines.

**Key dependencies:** data.table, collapse, gh, pipfun
(`pip_sign_save`, `load_from_gh`, `pip_create_globals`), pipload, joyn,
qs/fst/haven, dm (data model visualisation).

### Known Technical Debt

- `pip_update_all_aux()` should be deprecated in favour of `auto_aux_update()`.
  It also passes a `src` parameter to `pip_pop()` which no longer accepts it.
- `return_value()` (recursive dependency resolver) exists but is unused;
  `auto_aux_update()` uses flat lookup since the YAML lists transitive
  dependencies explicitly. Keep for potential future use.
- `pip_sna()` is a no-op stub; SNA data is loaded inline by `pip_gdp_update()`
  and `pip_pce_update()`.

## Key Deliverables

- R package (CRAN-style, installed from GitHub)

## Constraints

- The package is intended for reproducibility: auxiliary data must be
  hash-signed and vintaged so that any PIP run can be traced back to its
  exact input data.
- All raw data originates from `PIP-Technical-Team/aux_*` GitHub repos or
  external APIs (WDI, WEO, Maddison). No hardcoded local file paths for
  raw inputs.

## Current Focus

<!-- TODO: Add current focus when work begins -->
