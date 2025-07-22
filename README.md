
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipaux

<!-- badges: start -->

[![R-CMD-check](https://github.com/PIP-Technical-Team/pipaux/workflows/R-CMD-check/badge.svg)](https://github.com/PIP-Technical-Team/pipaux/actions?workflow=R-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/PIP-Technical-Team/pipaux/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PIP-Technical-Team/pipaux?branch=master)
<!-- badges: end -->

`pipaux` manages the auxiliary data used in the PIP workflow. It
basically does two things. \[1\] It updates the auxiliary data and makes
sure to keep it unchanged when it the raw data has not been modified.
\[2\] it loads the data into memory.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/pipaux")
```

``` r
library(pipaux)
```

Even though `pipaux` has more than 0 functions, most of its features can
be executed by only using functions `pipaux::load_aux` and
`pipaux::update_all_aux`, which wrappers of other measure-specific
functions.

## Loading data

Loading auxiliary data can be done using the function
`pipaux::load_aux`, whose first argument is the name of the `measure` to
be loaded. The measures available are **and **.

So, loading the `cpi` database could be done by typing,
`load_aux(measure = "cpi")` or `aux_cpi("load")`.

Remember to first set up your working release, so that `load_aux()` will
automatically load the file in the current release folder. For example,

``` r
pipfun::setup_working_release(release = "20250203")
#> ℹ PIP working release setup to 20250203-TEST
```

``` r
identical(load_aux(measure = "cpi"), aux_cpi("load"))
#> [1] TRUE
identical(load_aux(measure = "ppp"), aux_ppp("load"))
#> [1] TRUE
identical(load_aux(measure = "pop"), aux_pop("load"))
#> [1] TRUE
```

## Updating data

Updating the auxiliary data in PIP can be done either one measure at a
time or all measures together. Data could be updated using the function
`pipaux::aux_fun(measure = "...")` of any of the measure specific
functions. Or all auxiliary data measures can be updated altogether with
`update_all_aux`. Both of these functions also update any dependencies
of the specified measure.

Alternatively, updating the specified measure (e.g., cpi) only can be
done using again the `aux_cpi(action = "update")` function.

``` r
# this, 
aux_fun(measure = "cpi")

# is equivalent to this, 
aux_cpi("update")

# And this
update_all_aux(measure = "cpi")
```

`pipaux` checks whether the hash of each database has changed and
updates the data in case it has changed. If it has not changed, it
notifies the user. The user can force the update by using `force = TRUE`
as part of the arguments.
