#' Validate raw sna_fy data
#'
#' @param sna_fy raw sna_fy data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
sna_fy_validate_raw <- function(sna_fy, detail = getOption("pipaux.detail.raw")){

  stopifnot("sna_fy raw data is not loaded" = !is.null(sna_fy))

  report <- data_validation_report()

  validate(sna_fy, name = "sna_fy raw data validation") |>
    validate_if(is.character(Code),
                description = "`Code` should be character") |>
    validate_if(is.character(LongName),
                description = "`LongName` should be character") |>
    validate_if(is.character(SpecialNotes),
                description = "`SpecialNotes` should be character") |>
    validate_if(is.character(Month),
                description = "`Month` should be character") |>
    validate_cols(not_na, Code, Month, Day,
                  description = "no missing values in key variables") |>
    # validate_if(is_uniq(Code, LongName),
    #             description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
      setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

