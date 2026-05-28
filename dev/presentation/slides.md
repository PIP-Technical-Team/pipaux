# pipaux — Auxiliary Data Pipeline

## What we built, how it works, and why it matters

**PIP Technical Team** | May 2026

---

---

## Agenda

1. What is auxiliary data? — types, sources, and why it's complex
2. The data flow — from source to Y-drive
3. The dependency graph — how measures relate
4. The update engine — how pipaux decides what to do
5. Change detection & comparison — verifying what changed
6. Quick demo

---

---

## What is Auxiliary Data?

Auxiliary data feeds into the PIP poverty estimation pipeline.

### Three stages

| Stage | Description |
|-------|-------------|
| **Raw** | As received from source |
| **Input** | Validated & formatted |
| **Output** | Saved to Y-drive, used by PIP |

### Multiple sources

| Source | Examples |
|--------|----------|
| **Poverty GP** | PFW, CPI, PPP |
| **WDI** | GDP (chained with other sources) |
| **External** | Maddison, WEO |
| **Team-managed (GitHub)** | Population, NaN (nowcast) |

---

---

## Historical Challenges

| Challenge | Old world | pipaux |
|-----------|-----------|--------|
| Formatting raw data | Manually triggered | Automated per measure |
| Source selection | Ad-hoc (WDI? POP? depends…) | Codified in each `aux_*` function |
| Cross-stage checks | None | Validation at raw + output |
| Output dependencies | Tied to pipeline code | Dependency manifest (YAML) |
| Versioning | Not linked to releases | Release-scoped, content-based |
| Traceability | None | Full provenance (SHA, code hash, logs) |

---

---

## The Data Flow

> *[Show Diagram 1 — Data Flow]*

Sources at the top → GitHub repos in the middle → pipaux doing the work → Y-drive at the bottom.

"Data flows from multiple sources into GitHub repos — one repo per measure.
pipaux sits in the middle: it syncs branches, validates, detects changes,
and saves with full provenance to the Y-drive."

---

---

## The Dependency Graph

> *[Show Diagram 3 — Actual Dependencies]*

Key observations:

- **`country_list`** is the backbone — 10 measures depend on it
- **`pfw`** is the second hub — feeds 5 measures
- **`gdp`** is the most complex derived measure (7 inputs)
- **`missing_data`** sits at the bottom — depends on nearly everything
- Darker = more upstream dependencies

> "If `country_list` changes, most of the graph re-runs — automatically."

---

---

## Why Dependencies Matter

```r
# You don't need to think about ordering.
# pipaux resolves it automatically.

update_aux_measures(
  measures = c("gdp", "cpi"),
  log      = TRUE
)

# gdp depends on wdi, weo, maddison, sna, nan, pop
# → all updated first, in the right order
# → then gdp itself
```

> One command. All dependencies handled. No manual orchestration.

---

---

## The Update Engine

> *[Show Diagram 2 — Pipeline Architecture]*

Decision logic:

1. Read dependency manifest → topological sort
2. For each measure: already processed? → skip
3. GitHub check: release branch in sync with DEV? If not → sync
4. Y-drive check: SHA match? Code hash match? If all match → skip entirely
5. If mismatch → load, validate, format, save with provenance
6. Log every step

> "The smart part is the skip logic — if nothing changed, nothing happens.
> No unnecessary work, no silent overwrites."

---

---

## Checking Status — Quick Diagnostic Tool

```r
# Want to know if a measure needs updating?
# Use check_status() — no need to run the full update
check_status("maddison", include_reason = TRUE)
```

```
── Checking Status for maddison ────────────────────────────────────────
✔ GitHub branch 20260401_TEST is up to date with DEV.

── Summary ─────────────────────────────────────────────────────────────
ℹ Update GitHub: FALSE
ℹ Update Y drive: TRUE
```

```r
# Returns a list you can inspect programmatically
$update_gh
[1] FALSE

$update_y
[1] TRUE

$gh_reason
[1] "GitHub up to date"

$y_reason
[1] "Aux sidecar missing"
```

> Used internally by `update_aux_data()` — but also handy as a standalone utility.

---

---

## Change Detection — Three Signals

### 1. GitHub SHA

Raw file content changed on GitHub? → SHA mismatch detected → Re-sync + re-save

### 2. Code Hash

Did the `aux_*` function body change? → Hash mismatch detected → Re-format + re-save

### 3. Dependency Cascade

Did any upstream measure change? → Propagated automatically → All dependents re-run

> Nothing gets overwritten silently. Every change is intentional and traceable.

---

---

## Provenance — What Gets Saved

Every artifact on the Y-drive carries a sidecar with:

```
┌─────────────────────────────────────────────┐
│  data.qs2  +  sidecar metadata              │
│                                             │
│  gh:         owner, repo, branch, SHA       │
│  code_hash:  hash of aux_* function body    │
│  code_label: name of the formatter function │
│  pk:         primary key columns            │
└─────────────────────────────────────────────┘
```

→ You can always answer: *"What version of the raw data and what version
of the code produced this file?"*

---

---

## Versioning — Tied to Releases

```r
# Versioning modes
pipaux_set_versioning("content")    # default — new version only on change
pipaux_set_versioning("timestamp")  # new version every run (debugging)
pipaux_set_versioning("off")        # overwrite, no history
```

- Content-based: a new version is saved **only when data or code changes**
- Every version is scoped to a release (`20260202_PROD`)
- Full history retained — compare any two versions

---

---

## Post-Update: Comparing Changes

> *[Show Diagram 6 — Comparisons]*

Two tools:

- **`compare_aux_releases()`** — "What changed between this release and last?"
- **`compare_aux_vintages()`** — "Did my re-run change anything within this release?"

Both return:
- `diff_values`: cell-level differences
- `diff_rows`: added/removed rows

---

---

## Comparing Releases — Example

```r
changes <- compare_aux_releases(
  measure     = c("cpi", "gdp"),
  old_release = "20260101_PROD"
)

# What changed in CPI?
changes$cpi$diff_values   # cell-level differences
changes$cpi$diff_rows     # added/removed rows
```

```r
# Comparing within the same release (vintages)
vintage_changes <- compare_aux_vintages(
  measures = c("cpi", "gdp"),
  version  = -1  # vs immediately previous version
)
```

---

---

## Logging — Full Audit Trail

```r
# Quick summary: measure + status
aux_log_summary()
```

```
      measure   status
1:        cpi  success
2:        ppp  success
3:        gdp    error
4:        pop  success
...
```

```r
# Detailed log: all steps, timing, errors
aux_log_last()
```

```
── Log entries: ──

→ [2026-05-28 13:59:18.211036] INFO - `Starting status check for: maddison`
Function: `aux_fun(measure = measure, repo = repo, owner = owner, processed = processed, tag = tag, log = log, log_overwrite = log_overwrite, verbose = verbose, halt_on_dep_fail = halt_on_dep_fail, log_name = log_name)` (from )
Trace: aux_fun(measure = measure, repo = repo, owner = owner, processed = processed, , tag = tag, log = log, log_overwrite = log_overwrite, verbose = verbose, , and halt_on_dep_fail = halt_on_dep_fail, log_name = log_name)
Metadata: list(step = "START", measure = "maddison")

→ [2026-05-28 14:00:07.466638] INFO - `No update needed for: maddison`
Function: `aux_fun(measure = measure, repo = repo, owner = owner, processed = processed, tag = tag, log = log, log_overwrite = log_overwrite, verbose = verbose, halt_on_dep_fail = halt_on_dep_fail, log_name = log_name)` (from )
Trace: aux_fun(measure = measure, repo = repo, owner = owner, processed = processed, , tag = tag, log = log, log_overwrite = log_overwrite, verbose = verbose, , and halt_on_dep_fail = halt_on_dep_fail, log_name = log_name)
Metadata: list(step = "END", measure = "maddison")
```

Every step is logged with: timestamp, level, message (backtick-wrapped), function signature, trace, and metadata (step + measure).

---

---

## Quick Demo

```r
# 1. Set up release
pipfun::setup_working_release(release = "20260202", identity = "TEST")

# 2. Update measures
update_aux_measures(measures = "cpi", log = TRUE)

# 3. Check status
aux_log_summary()

# 4. Compare with previous release
compare_aux_releases(measure = "cpi")
```

4 commands — that's the entire workflow.

---

---

## Summary

### What pipaux does

- Updates 20+ aux measures automatically
- Resolves dependencies in the right order
- Detects changes across 3 signals
- Saves with full provenance
- Compares across releases and vintages

### Key takeaways

1. **Automated & traceable** — one command updates everything with full logs
2. **Change-detected** — nothing gets overwritten silently
3. **Dependency-aware** — ordering is handled, not manual

---

---

## Questions?

> `update_aux_measures()` — one command to rule them all
