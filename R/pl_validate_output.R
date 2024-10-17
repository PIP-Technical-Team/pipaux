#' Validate output pl data
#'
#' @param pl output pl data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pl_validate_output <- function(pl, detail = getOption("pipaux.detail.output")){

  stopifnot("PL clean data is not loaded" = !is.null(pl))

  report <- data_validation_report()

  validate(pl, name = "PL output data validation") |>
    validate_if(is.character(name),
                description = "`name` should be character") |>
    validate_if(is.numeric(poverty_line),
                description = "`poverty_line` should be numeric") |>
    validate_if(is.logical(is_default),
                description = "`is_default` should be logical") |>
    validate_if(is.logical(is_visible),
                description = "`is_visible` should be logical") |>
    validate_if(is.integer(ppp_year),
                description = "`ppp_year` should be numeric") |>
    validate_cols(not_na, name, ppp_year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(name, ppp_year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
