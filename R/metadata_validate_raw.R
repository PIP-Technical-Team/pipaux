#' Validate raw metadata data
#'
#' @param metadata raw metadata data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
metadata_validate_raw <- function(metadata, detail = getOption("pipaux.detail.raw")){

  stopifnot("metadata raw data is not loaded" = !is.null(metadata))

  report <- data_validation_report()

  validate(metadata, name = "metadata raw data validation") |>
    validate_if(is.character(status),
                description = "`status` should be character") |>
    validate_if(is.character(reg),
                description = "`reg` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAR", "SSA")),
                  reg, description = "`reg` values within range") |>
    validate_if(is.numeric(id),
                description = "`id` should be numeric") |>
    validate_if(is.character(svy_id),
                description = "`svy_id` should be character") |>
    validate_if(is.character(link),
                description = "`link` should be character") |>
    validate_if(is.character(title),
                description = "`title` should be character") |>
    validate_if(is.character(data_access),
                description = "`data_access` should be character") |>
    validate_if(is.numeric(year_start),
                description = "`year_start` should be numeric") |>
    validate_if(is.numeric(year_end),
                description = "`year_end` should be numeric") |>
    validate_if(is.character(authoring_entity_name),
                description = "`authoring_entity_name` should be character") |>
    validate_if(is.character(authoring_entity_affiliation),
                description = "`authoring_entity_affiliation` should be character") |>
    validate_if(is.character(contact_email),
                description = "`contact_email` should be character") |>
    validate_if(is.character(contact_uri),
                description = "`contact_uri` should be character") |>
    validate_if(is.character(abstract),
                description = "`abstract` should be character") |>
    validate_if(is.character(collection_dates_cycle),
                description = "`collection_dates_cycle` should be character") |>
    validate_if(is.character(collection_dates_start),
                description = "`collection_dates_start` should be character") |>
    validate_if(is.character(collection_dates_end),
                description = "`collection_dates_end` should be character") |>
    validate_if(is.character(coverage),
                description = "`coverage` should be character") |>
    validate_if(is.character(sampling_procedure),
                description = "`sampling_procedure` should be character") |>
    validate_if(is.character(collection_mode),
                description = "`collection_mode` should be character") |>
    validate_if(is.character(coll_situation),
                description = "coll_situation` should be character") |>
    validate_if(is.character(weight),
                description = "`weight` should be character") |>
    validate_if(is.character(cleaning_operations),
                description = "`cleaning_operations` should be character") |>
    validate_if(is.character(coverage_notes),
                description = "`coverage_notes` should be character") |>
    validate_cols(not_na, svy_id,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(svy_id),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

