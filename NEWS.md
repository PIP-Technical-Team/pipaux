# pipaux (development version)

- Read indicators.csv from GH repo aux_indicators. It reads only from main branch. parallel development reads from DEV and PROD branches.

- Include variables with "_label" suffics in inequality chart data of country profiles. Requested by ITS



# pipaux 0.0.8

-   Retrieve SNA data from github repo, not from drive anymore

-   Store WDI in drive to avoid unexpected changes

-   Update chain factor in GDP update

# pipaux 0.0.7

-   Add `ppp_vintage` database when `ppp` is updated.
-   Use [Github repo](https://github.com/PIP-Technical-Team/pip-metadata) to
    retrieve metadata rather than internal folder
-   Use Github to retrieve special national accounts data
-   Make sure WEO is updated each time gdp data is updated
-   Slightly modify loading of options in `zzz.R`

# pipaux 0.0.6

## Enhancements

-   Add handling of more poverty lines and multiple increments to `pip_pl()`
-   Add `pip_dictionary()` to handle a dictionary of all PIP API response
    columns

## Bug fixes

-   Add various bug fixes for `pip_pop_update()`
-   Add reporting level column to CP charts databases

# pipaux 0.0.5

## Enhancements

-   update `pip_sign_save()` function to make it work with the RSconnect server.

# pipaux 0.0.4

## Enhancements

-   change all directory paths from `paste0()` to `fs::path()`

## Bug fixes

-   fix bugs on installation
-   fix bugs from `devtools::check()`

# pipaux 0.0.3

## Enhancements

-   Add handling of non-calendar years in WDI data for GDP and PCE. These are
    now converted to fiscal year values. #62

## Bug fixes

-   In the survey metadata file set Set distribution_type to "micro, imputed"
    for imputed surveys\
-   Add survey year column to the survey metadata file \# pipaux 0.0.3

Initial release used in the PIP soft-launch on February 9, 2022 \# pipaux 0.0.2

-   load global variables in `gls` using `pipload::pip_create_globals()` and
    `pipload::add_gls_to_env()`

# pipaux 0.0.1

-   Add global variables from `pipload::pip_create_globals`

-   Added a `NEWS.md` file to track changes to the package.
