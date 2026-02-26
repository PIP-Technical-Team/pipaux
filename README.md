
# pipaux

<!-- badges: start -->

[![R-CMD-check](https://github.com/PIP-Technical-Team/pipaux/workflows/R-CMD-check/badge.svg)](https://github.com/PIP-Technical-Team/pipaux/actions?workflow=R-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/PIP-Technical-Team/pipaux/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PIP-Technical-Team/pipaux?branch=master)
<!-- badges: end -->

`pipaux` manages auxiliary data used in the PIP workflow.

It provides tools to:

- Update auxiliary measures in dependency-aware order  
- Synchronize GitHub release branches and Y-drive outputs  
- Log updates (timing, status, errors)  
- Compare data across releases and vintages

------------------------------------------------------------------------

## Installation

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/pipaux")

library(pipaux)
```

Quick Start 1. Set up working release

All operations are tied to a release:

``` r
# Set the working release
set_working_release("release-1.0")
```

2.  Update auxiliary data

``` r
update_aux_measures(
  measures = NULL,              # NULL = all measures
  owner    = "PIP-Technical-Team",
  log      = TRUE
)
```

What this does:

- Resolves dependencies automatically

- Updates GitHub release branches (if needed)

- Writes processed files to the working directory

- Logs status, runtime, and errors

Inspect last log:

``` r
aux_log_last()
```

3.  Load auxiliary data

``` r
pipload::load_aux_data(
  measures = c("cpi", "pop")
)
```

Wrapper around measure-specific loaders (e.g. aux_cpi(“load”)).

4.  Compare data

## Across releases

``` r
compare_aux_releases("cpi")
```

## Within release (vintages)

``` r
compare_aux_vintages(
  measures = "cpi",
  version  = -1
)
```

**Outputs highlight**:

- Added/removed rows

- Changed values

- Key columns used for comparison

**Design Principles**

- Dependency-aware processing

- Release-scoped operations

- Transparent logging

- Minimal high-level API
