
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipaux

<!-- badges: start -->

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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pipaux)
# Load gdp data
df <- pip_gdp("load")

# Load CPI data
df <- pip_cpi("load")

# Load population data
df <- pip_pop("load")
```
