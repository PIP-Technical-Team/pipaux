#' Validate npl raw data
#'
#' @param npl raw npl data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
npl_validate_raw <- function(npl, detail = getOption("pipaux.detail.raw")){

  stopifnot("NPL raw data is not loaded" = !is.null(npl))

  report <- data_validation_report()

  validate(npl, name = "NPL raw data validation") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    # validate_cols(in_set(c("AFE", "AFW", "EAP", "ECA", "LAC", "MNA", "SAR")),
    #               region, description = "`region` values within range") |>
    validate_if(is.character(countrycode),
                description = "`countrycode` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(vsi_pov_nahc_nc),
                description = "`vsi_pov_nahc_nc` should be numeric") |>
    validate_if(is.numeric(vsi_pov_nahc),
                description = "`vsi_pov_nahc` should be numeric") |>
    validate_if(is.numeric(comparability),
                description = "`comparability` should be numeric") |>
    validate_if(is.character(footnote),
                description = "`footnote` should be character") |>
    validate_cols(not_na, countrycode, year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(countrycode, year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
