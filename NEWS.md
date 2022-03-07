# pipaux 0.0.6

## Enhancements

* Add handling of more poverty lines and mulitiple increments to `pip_pl()`

## Bug fixes

* Add various bug fixes for `pip_pop_update()`
* Add reporting level column to CP charts datasets 

# pipaux 0.0.5

## Enhancements

* update `pip_sign_save()` funciton to make it work with the RSconnect server.

# pipaux 0.0.4

## Enhancements

* change all directory paths from `paste0()` to `fs::path()`

## Bug fixes

* fix bugs on installation
* fix bugs from `devtools::check()`

# pipaux 0.0.3

## Enhancements

* Add handling of non-calendar years in WDI data for GDP and PCE. These are now converted to fiscal year values. #62

## Bug fixes
 
* In the survey metadata file set Set distribution_type to "micro, imputed" for imputed surveys  
* Add survey year column to the survey metadata file 
# pipaux 0.0.3

Initial release used in the PIP soft-launch on February 9, 2022
# pipaux 0.0.2

* load global variables in `gls` using `pipload::pip_create_globals()` and `pipload::add_gls_to_env()`

# pipaux 0.0.1

* Add global variables from `pipload::pip_create_globals`

* Added a `NEWS.md` file to track changes to the package.
