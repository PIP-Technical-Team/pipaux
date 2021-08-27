
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipaux

<!-- badges: start -->
[![R-CMD-check](https://github.com/PIP-Technical-Team/pipaux/workflows/R-CMD-check/badge.svg)](https://github.com/PIP-Technical-Team/pipaux/actions?workflow=R-CMD-check)
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

Even though `pipaux` has more than 20 functions, most of its features
can be executed by only using functions `pipaux::load_aux` and
`pipaux::update_aux`, which wrappers of other measure-specific
functions.

## Loading data

Loading auxiliary data can be done using the function
`pipaux::load_aux`, whose first argument is the name of the `measure` to
be loaded. The measures available are **cpi, gdp, pce, pfw, pop, and
ppp**.

So, loading the `cpi` database could be done by typing,
`load_aux(measure = "cpi")` or `pip_cpi("load")`.

``` r
identical(load_aux(measure = "cpi"), pip_cpi("load"))
#> [1] TRUE
identical(load_aux(measure = "ppp"), pip_ppp("load"))
#> [1] TRUE
identical(load_aux(measure = "pop"), pip_pop("load"))
#> [1] TRUE
```

## Updating data

Updating the auxiliary data in PIP uses the same syntax as loading it.
Data could be updated using the function `pipaux::load_aux` of any of
the measure specific functions. Before each version of `pipaux` is
released, it is verified whether there is any auxiliary database that is
not up to date. So, these functions are seldom used.

``` r
# this, 
update_aux(measure = "cpi")
#> Data signature is up to date.
#> No update performed

# is equivalent to this, 
pip_cpi("update")
#> Data signature is up to date.
#> No update performed

# And this
update_aux(measure = "ppp")
#> Data signature is up to date.
#> No update performed

# is equivalent to this, 
pip_ppp("update")
#> Data signature is up to date.
#> No update performed
```

`pipaux` checks whether the hash of each database has changed and
updates the data in case it has changed. If it has not changed, it
notifies the user. The user can force the update by using `force = TRUE`
as part of the arguments.
