# pipaux — Copilot guide (technical)

Target audience
- PIP Technical Team maintaining or running the PIP auxiliary-data workflow. Assumes access to PIP servers and a GitHub PAT.

Purpose
- Orchestrates loading, validating, transforming, saving and comparing auxiliary measures used by the PIP ingestion pipeline. Measure-specific formatters live in `R/aux_*.R`; update orchestration and change-detection live in `R/update_aux_data.R` and `R/identify_changes.R`.

Table of contents
- Purpose
- Quickstart
- Core concepts (.pipaux)
- GH provenance & raw SHA semantics
- Check → Update process (detailed, `update_aux_data.R`)
- Logging (what, when, where)
- Per-measure notes (verified)
- Troubleshooting
- README note
- Appendix: useful commands and checks

---

## Quickstart — precise steps (developer)
1. From package root start R and load sources:

```r
devtools::load_all()
```

2. MANDATORY: set the working release (this populates `.pipaux` used by all flows):

```r
pipfun::setup_working_release()
```

3. Inspect essential runtime objects:

```r
ls(envir = .pipaux)
get_from_auxenv("wrk_release")
get_from_auxenv("aux_data_path")
```

4. Read a saved auxiliary measure (read-only), current version by default:

```r
pipload::load_aux_data(measure = "gdp")
```

5. Update a single measure (handles dependencies):

```r
aux_fun(measure = "gdp", force = FALSE, verbose = TRUE)
```

6. Update all measures (top-level orchestration): use `update_all_aux()` or the wrapper defined in `R/update_aux_data.R`.

7. Compare releases / vintages:

```r
compare_aux_releases(old_release = "YYYYMMDD_ID")
compare_aux_vintages(measures = c("cpi","gdp"), version = -1)
```

---

## Core concepts and runtime objects

`.pipaux` environment (populated by `pipfun::setup_working_release()` in `R/zzz.R`)
- `.pipaux$wrk_release`: list with `release` (YYYYMMDD) and `identity` (TEST/PROD)
- `.pipaux$pip_folders`: list of Y-drive and repository paths from `pipfun::get_pip_folders()`
- `.pipaux$aux_data_path`, `.pipaux$aux_metadata_path`: concrete paths for QS/aux artifacts
- `.pipaux$aux_alias`, `.pipaux$aux_meta_alias`: stamp alias for aux data and aux metadata folders
- Release-scoped options set in `zzz.R` (for example: `pipaux.madsrc`, `pipfun.ghowner`, `pipaux.pppyear`)
---

## GH provenance & raw SHA semantics (global)

- `pipfun::load_from_gh()` typically returns objects that include `attributes(x)$gh` — a list containing GitHub metadata such as the raw file SHA (commonly under some field like `raw_sha` depending on the loader). The presence of `attributes(x)$gh` is the canonical indicator that the input came from GitHub.
- The update pipeline stores GH provenance for saved artifacts via `pip_aux_save(..., metadata = list(gh = <gh>))`. The saved artifact metadata is later used to compare with newly loaded inputs.
- For derived measures (final object not read directly from GH) the pipeline must instead inspect input artifacts' `attributes(... )$gh` values or use a function digest (see `raw_sha_fun`) or content diffs to decide whether to republish the derived artifact.
- Canonical attribute keys used across the codebase: `gh` (GitHub metadata), `raw_sha_fun` (digest of the formatter function body), and `aux_key` (primary key columns). The authoritative use and exact names are implemented in `R/utils.R` and `R/update_aux_data.R`.
---

## Check → Update process (detailed)
This is implemented in `R/update_aux_data.R`. The sequence below precisely explains how a top-level update call proceeds.

1) Entry & top-level initialization
- `aux_fun()` is the typical entrypoint (or a wrapper such as `update_all_aux()`). If called from the top-level `is_top_level()` returns `TRUE`, then the run state and the update log are initialised/rotated and a `processed` set/environment is created to track completed measures in the cascade.

2) Resolve repo / branch / tag
- `resolve_measure_repo_owner()` computes `owner`, `repo`, `branch` and `tag` for the measure. By convention the `branch` is `paste0(release, "_", identity)`; code converts `branch == "main"` to `""` for the loaders.

# todo: add for which measures this is actually needed, for which owner is specific 

3) Dependency graph & recursion
- `read_dependencies()` loads the dependency map as list. `process_dependencies()` builds a graph and recursively calls `aux_fun()` for dependencies. The `processed` environment prevents repeated work and breaks cycles.

4) Per-measure update steps
- Load sources:
  - If measure uses GH raw inputs: use `pipfun::load_from_gh(...)` — the returned object normally contains `attributes(x)$gh` with GH raw SHA.
  - If derived: read inputs with `pipload::load_aux_data(...)` (these saved artifacts may include `metadata$gh`).
  - If external download: the loader may use `download.file()` / `read_*` and will not produce `attributes(x)$gh` unless the code explicitly sets a digest attribute.
- Validation: measure-specific raw validators run and abort on errors. Validation functions live alongside the `aux_*` formatters (e.g., `*_validate_raw`, `*_validate_output`).
- Transform & set attributes: formatter must set at least:
  - `attributes(result)$aux_key` (primary key columns)
  - optionally `attributes(result)$raw_sha_fun` (formatter function digest) if code-change should trigger republishing
  - preserve `attributes(input)$gh` when inputs are GH raw
- Decide whether to publish (`execute_update()`):
  - If `force == TRUE` → publish.
  - Otherwise compare provenance/sha/diffs:
    - If GH raw input present: compare `attributes(input)$gh` raw SHA vs saved artifact `metadata$gh` (elementwise for multi-file inputs).
    - For derived outputs: inspect inputs' `attributes(... )$gh` and/or `attributes(result)$raw_sha_fun`.
    - Optionally run a content diff (myrror) between new result and the saved Y-drive artifact to detect changes.
  - If any check indicates change → publish; else skip.
- Publication sequence:
  - `save_aux_to_gh()` writes a final file to an `aux_<measure>` GitHub repo and returns GH metadata — but this step is conditional and is only performed by measures whose code explicitly calls `save_aux_to_gh()` (or otherwise publishes to GH). Many measures only write the artifact to the Y‑drive and do not publish a GH file.
    - How to tell: search the measure's `R/aux_<measure>.R` implementation for `save_aux_to_gh(` or for a `pipfun::load_from_gh()` / `pipfun::save_to_gh()` pattern. If present, the measure maintains a GH copy and the pipeline will capture GH metadata.
    - Examples (inferred): `aux_country_list`, `aux_income_groups` (CLASS repo, `owner = "GPID-WB"`), `aux_nan` (owner often `PIP-Technical-Team`), `aux_dictionary`, `aux_cp` (multi-file), `aux_pfw` and some source-specific measures. Measures that aggregate/derive (for example `aux_gdp`) may or may not publish a GH copy depending on configuration and repo availability — check the measure file.
  - After GH publish (if done), `pip_aux_save(x = result, id = measure, metadata = list(gh = <gh>), code = <formatter>, pk = <aux_key>)` saves the artifact to Y‑drive and persists the sidecar metadata used for future comparisons.

5) Post-publish housekeeping and logging
- Mark measure as processed in the local cascade state, append success or failure to the log, and return saved metadata invisibly. On failure the log includes validation errors, I/O errors, and stack context.
# add how logs are saved in the update_all_aux function

6) Implementation details & gotchas
- Multi-file inputs: code usually collects per-file GH attributes into a list (e.g., `gh_list`) and writes that into `metadata$gh`. Comparison must be elementwise: a change in any input should trigger a refresh.
- Function digest: many `aux_*` functions compute `raw_sha_fun <- digest::digest(body(get(paste0("aux_", measure))))` (or similar). Persist that as `attributes(result)$raw_sha_fun` or `metadata$raw_sha_fun` to detect code changes.
- Content diffs: `R/identify_changes.R` and `myrror` utilities are used when SHA checks are inconclusive or to produce human-readable diffs.

Sidecar metadata & `pip_write` / `pip_aux_save`
- Recent changes centralised artifact metadata into a sidecar (rather than relying solely on R-level attributes attached to the in-memory object). In practice:
  - `pip_aux_save()` / `pip_write()` accept `metadata = list(...)` and `code = <function>` (or `code_label`) arguments and write a sidecar metadata file alongside the stored artifact (the sidecar contains GH provenance, function digest, aux_key, and other fields).
  - The pipeline should read provenance and code-digest information from the sidecar (via `pipload::pip_read()` or `pip_aux_read()` helpers) rather than assuming `attributes(x)$gh` will always be present on an in-memory object.
  - When writing, measures should pass `metadata = list(gh = <gh>)` and provide the `code` argument so the sidecar records which formatter was used; this makes comparisons robust across R sessions and languages.

Note: the code still preserves or uses `attributes(x)$gh` when available (for objects loaded directly from GH). However, the source of truth for saved artifacts is the sidecar metadata persisted by `pip_aux_save()` / `pip_write()`.
---

## Logging — what, when, where

Initialization
- On package load (via `.onLoad` in `R/zzz.R`) the pipaux logging backend initialises a named log (commonly `"pipaux_update_log"`) and an internal `.piplogenv` is used for runtime storage.

When entries are written
- On top‑level runs (is_top_level() TRUE), `aux_fun()` rotates/initialises the update log and writes run‑start metadata (release, identity, timestamp).
- Per‑measure: start, validation outcomes, detected changed inputs (GH raw SHA list), publish/skipped decision, and publish results (saved paths, GH raw SHA recorded) are appended to the log.
- On errors/warnings: validation failures, GH or Y‑drive I/O errors, and comparison mismatches are logged with context and stack traces where available.

Where logs live and how to read them
- Runtime: logs are kept in the pipfun logging backend and in the session environment (`.piplogenv` / `.pipaux` logging entries).
- Retrieve logs programmatically:
```r
devtools::load_all()
pipfun::setup_working_release()
pipfun::log_get("pipaux_update_log")
```
- The log object contains per‑measure entries (timestamps, severity, messages, metadata). The exact persistent storage/backing file depends on the `pipfun` logging backend; check `pipfun` docs or `pipfun::log_info` helpers for how logs are written to disk/central storage in your environment.

Best practice
- Run updates top‑level (so logs are initialised once).
- Inspect `pipfun::log_get("pipaux_update_log")` after runs for quick troubleshooting.
- Use `verbose = TRUE` in `aux_fun()` / `update_all_aux()` to see synchronous console messages while the log retains the structured record.

---

## Per-measure verified notes (alphabetical)
Each entry below indicates: Source Type (GH-raw / Derived / Static), exact loader/save snippets (where available), attribute handling (`gh`, `raw_sha_fun`, `aux_key`), and recommendations.

### `aux_censoring.R`
- Source type: GH-raw primary (reads `countries.csv` and `regions.csv` via `pipfun::load_from_gh()`).
- Loader lines:
```r
countries <- pipfun::load_from_gh(measure = measure, owner = owner, branch = branch, filename = "countries.csv")
regions   <- pipfun::load_from_gh(measure = measure, owner = owner, branch = branch, filename = "regions.csv")
```
- Attributes set and saved:
```r
raw_sha_fun <- digest::digest(body(paste0("aux_", measure)))
setattr(dl, "raw_sha_fun", raw_sha_fun)
setattr(dl, "aux_key", c("countries","regions"))
saved <- pip_aux_save(x = dl, id = measure, force = force, code = aux_censoring, ...)
```
- Notes: inputs carry `attributes(...)$gh`; update logic compares per-file GH raw SHAs against saved metadata. `raw_sha_fun` captures code changes.

### `aux_countries.R`
- Source type: Derived (subsets `country_list` and `pfw` via `pipload::load_aux_data()`).
- Loader lines:
```r
cl  <- pipload::load_aux_data(measure = "country_list")
pfw <- pipload::load_aux_data(measure = "pfw")
```
- Attributes and save:
```r
setattr(countries, "aux_name", "countries")
setattr(countries, "aux_key", c("country_code"))
raw_sha_fun <- digest::digest(body(paste0("aux_", measure)))
setattr(countries, "raw_sha_fun", raw_sha_fun)
pip_aux_save(x = countries, id = measure, pk = key_cols, code = aux_countries, ...)
```
- Notes: final object is derived; decisions to republish must consider `attributes(cl)$gh` and `attributes(pfw)$gh` on inputs or `raw_sha_fun` changes.

### `aux_country_list.R`
- Source type: GH-raw primary (CLASS dataset via `pipfun::load_from_gh()` from `GPID-WB/Class`).
- Loader & attributes:
```r
dt <- pipfun::load_from_gh(measure = measure, owner = "GPID-WB", repo = "Class", branch = class_branch, filename = "OutputData/CLASS", ext = "dta")
gh <- attributes(dt)$gh
setattr(dt, "aux_key", c("country_code"))
setattr(dt, "raw_sha_fun", raw_sha_fun)
pip_aux_save(x = dt, id = measure, pk = key_cols, metadata = list(gh = gh), ...)
```
- Notes: the function explicitly persists `metadata$gh`; compare logic reads saved metadata vs newly loaded attributes.

### `aux_cp.R` (Country Profiles)
- Source type: GH-raw multi-file (reads many CSV/DTA files via `pipfun::load_from_gh()`).
- Key patterns:
```r
raw_files <- purrr::map(file_names, ~ pipfun::load_from_gh(measure = "cp", owner = owner, branch = branch, filename = .x, ext = "csv"))
raw_fl    <- purrr::map(fl_files,   ~ pipfun::load_from_gh(..., ext = "dta"))
gh_list <- lapply(raw_files, function(x) attributes(x)$gh)
setattr(dl, "raw_sha_fun", raw_sha_fun)
setattr(dl, "aux_key", names(dl))
pip_aux_save(x = dl, id = measure, metadata = list(gh = gh_list), ...)
```
- Notes: saved artifact stores per-file `gh` list; a change in any element should trigger republish.

### `aux_cpi.R`
- Source type: GH-raw primary for many CPI inputs; also uses `country_list` and other aux inputs.
- Special: month helpers (`get_month_number`, `days_in_month`) and decimal/precision validators — preserve month metadata and `aux_key`.

### `aux_dictionary.R`
- Source type: GH-raw (dictionary CSV via `pipfun::load_from_gh()`).
- Save includes: `metadata = list(gh = attributes(df)$gh)` and `attributes(df)$raw_sha_fun` for code-change detection.

### `aux_gdm.R`
- Source type: GH-raw primary but merges with derived inputs (`pfw`, inventory via `fst` reads).
- Key snippet:
```r
df <- pipfun::load_from_gh(measure = "gdm", owner = owner, branch = branch, ext = "csv")
gh <- attr(df, "gh")
setattr(df, "gh", gh)
pip_aux_save(x = df, id = measure, metadata = list(gh = gh), ...)
```

### `aux_gdp.R`
- Source type: Derived composite (aggregates WDI, WEO, Maddison, SNA, NAN, population). Inputs may be a mix of `pipload::load_aux_data()` saved artifacts and direct `pipfun::load_from_gh()` reads.
- Publication flow:
  - `save_aux_to_gh(df = gdp, ...)` writes a CSV to the `aux_gdp` GitHub repo.
  - `gdp_gh <- pipfun::load_from_gh(measure = "gdp", ...)` reads the published GH file; then `pip_aux_save(..., metadata = list(gh = attributes(gdp_gh)$gh))` persists GH metadata to Y-drive.
- Notes: update decisions must inspect both published final GH SHA and input artifacts' metadata; consider adding `raw_sha_fun` to detect formatter code changes.

### `aux_income_groups.R`
- Source type: GH-raw (GPID-WB/Class). Persists `attributes(ig)$gh` to metadata and records `raw_sha_fun`.

### Remaining measures (summarized)
- `aux_indicators.R`: GH-raw (indicators CSV/JSON). Persist `metadata$gh`.
- `aux_labels_pip.R`: static labels (internal) or GH-raw depending on loader. Preserve encodings.
- `aux_maddison.R`: external URL (option `pipaux.madsrc`), typically not GH-raw; consider adding a download digest attribute.
- `aux_metadata.R`: derived from `pfw` and `country_list`. Preserve inputs' provenance and set `raw_sha_fun`.
- `aux_missing_data.R`: derived diagnostic (no GH raw on output). Preserve inputs' provenance if needed.
- `aux_nan.R`: GH-raw (PIP-Technical-Team) or internal fallback. Keep `attributes(... )$gh` intact.
- `aux_npl.R`, `aux_pl.R`, `aux_pce.R`, `aux_pfw.R`, `aux_pop.R`, `aux_ppp.R`, `aux_regions.R`, `aux_sna.R`, `aux_wdi.R`, `aux_weo.R`: follow the same patterns described above — loaders via `pipfun::load_from_gh()` when GH source exists (produces `attributes(x)$gh`), or `pipload::load_aux_data()` for derived inputs.

---

## Troubleshooting (common GH raw SHA & update issues)

- After loading a source, immediately inspect GH provenance and function digest:
```r
str(attributes(obj)$gh)
str(attributes(obj)$raw_sha_fun)
```
- If `attributes(... )$gh` is missing where expected:
  - Confirm the loader used `pipfun::load_from_gh()` and not a local `read_*()` call.
  - Inspect `R/aux_data_files.R` to see how the loader path for that measure is resolved.
- If Y-drive artifact did not update when GH changed:
  - Confirm `pip_aux_save(..., metadata = list(gh = gh))` was called and metadata persisted.
  - Check `update_aux_data.R` comparison logic: it must compare the exact field name used inside `attributes(x)$gh` (for example `raw_sha`) against the saved metadata.
- If `identify_changes()` fails to find keys:
  - Confirm `attributes(result)$aux_key` or `stamp::st_get_pk(result)` returns the expected primary key columns.

---

## README note
- The repository `README.md` is outdated. Replace it with a short developer-focused README containing: purpose, Quickstart (load_all + setup_working_release), update/compare commands, GH raw SHA semantics, and a link to this `copilot.md`. I can draft and commit that if you want.

---

## Appendix — developer checks & snippets

- Inspect `.pipaux` and key paths:
```r
devtools::load_all()
pipfun::setup_working_release()
ls(envir = .pipaux)
get_from_auxenv("wrk_release")
get_from_auxenv("aux_data_path")
```

- Run and inspect a single update with verbose logging:
```r
devtools::load_all()
pipfun::setup_working_release()
aux_fun("gdp", verbose = TRUE, force = FALSE)
pipfun::log_get("pipaux_update_log")
```

- Generate dependency diagram (save to `inst/images/dependencies.png`):
```r
devtools::load_all()
deps <- read_dependencies()
dir.create("inst/images", recursive = TRUE, showWarnings = FALSE)
png("inst/images/dependencies.png", width = 1600, height = 1200, res = 150)
plot_dependencies(deps)
dev.off()
```

---
