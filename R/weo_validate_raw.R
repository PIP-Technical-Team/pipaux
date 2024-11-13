#' Validate raw weo data
#'
#' @param weo raw weo data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
weo_validate_raw <- function(weo, detail = getOption("pipaux.detail.raw")){

  stopifnot("WEO raw data is not loaded" = !is.null(weo))

  report <- data_validation_report()

  weo <- weo[!is.na(`WEO Subject Code`), ]

  validate(weo, name = "WEO raw data validation") |>
    validate_if(is.character(`WEO Country Code`),
                description = "`WEO Country Code` should be character") |>
    validate_if(is.character(ISO),
                description = "ISO should be character") |>
    validate_if(is.character(`WEO Subject Code`),
                description = "`WEO Subject Code` should be character") |>
    validate_if(is.character(Country),
                description = "`Country` should be character") |>
    validate_if(is.character(`Subject Descriptor`),
                description = "`Subject Descriptor` should be character") |>
    validate_if(is.character(`Subject Notes`),
                description = "`Subject Notes` should be character") |>
    validate_if(is.character(Units),
                description = "`Units` should be character") |>
    validate_if(is.character(Scale),
                description = "`Scale` should be character") |>
    validate_if(is.character(`Country/Series-specific Notes`),
                description = "`Country/Series-specific Notes` should be character") |>
    validate_if(is.numeric(`Estimates Start After`),
                description = "`Estimates Start After` should be numeric") |>
    validate_cols(not_na, ISO, `WEO Subject Code`,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(ISO, `WEO Subject Code`),
                description = "no duplicate records in key variables") |>
    add_results(report)

  num_var_list <- grep("^[[:digit:]]", colnames(weo))

  for (i in 1:length(num_var_list)) {
    validate(weo, name = "WEO validation") |>
      validate_cols(is.numeric, num_var_list[i],
                    description = "variables (with numeric var name) should be numeric") |>
      add_results(report)
  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

