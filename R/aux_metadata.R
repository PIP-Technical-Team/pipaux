#' PIP Survey Metadata
#'
#' Update or load a dataset with survey metadata.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @export
aux_metadata <- function(action  = c("update", "load"),
                         owner   = getOption("pipfun.ghowner"),
                         tag     = NULL,
                         verbose = FALSE,
                         detail  = getOption("pipaux.detail.raw")) {
  measure <- "metadata"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {

    aux_metadata_update(
      owner   = owner,
      branch  = branch,
      tag     = tag,
      detail  = detail
    )

  } else {

    pipload::load_aux_data(measure = measure, verbose = verbose)

  }
}

#' Update metadata file
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pipfun::load_from_gh
#' @inheritParams aux_metadata
#' @return logical. TRUE if saved correctly. FALSE if error happened
#' @export
aux_metadata_update <- function(owner   = getOption("pipfun.ghowner"),
                                branch  = NULL,
                                tag     = branch,
                                verbose = FALSE,
                                detail  = getOption("pipaux.detail.raw")) {

  measure <- "metadata"

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  df <- pipfun::load_from_gh(measure = measure,
                             owner = owner,
                             branch = branch,
                             tag = tag,
                             ext = "csv")

  gh <- attributes(df)$gh

  # validate raw metdata data
  metadata_validate_raw(metadata = df, detail = detail)

  # Load pfw
  pfw <- pipload::load_aux_data(measure = "pfw", verbose = verbose)

  # Create distribution type column (data type)

  pfw[,
      domain_check := (gdp_domain == 2 | pce_domain == 2 |
                         pop_domain == 2 | cpi_domain == 2 |
                         ppp_domain == 2)]

  # order  matters here
  pfw[,
      distribution_type  := fcase(
        use_imputed   == 1, "micro, imputed",
        use_microdata == 1, "micro",
        use_groupdata == 1 & domain_check, "aggregated",
        use_groupdata == 1, "group",
        default = NA_character_
      )
  ]

  # Merge datasets (inner join)
  df <-
    merge(df,
          pfw[, c("country_code", "ctryname", "surveyid_year", "survey_acronym",
                  "welfare_type", "reporting_year", "distribution_type",
                  "surv_producer","survey_coverage", "surv_title",
                  "link", "survey_year")],
          by = "link", all.y = TRUE
    )

  # Recode colnames
  setnames(x = df,
           old = c("title", "surv_producer", "ctryname"),
           new = c("survey_title", "survey_conductor", "country_name"))
  df[,
     survey_title := fifelse(is.na(survey_title), surv_title, survey_title)
  ]

  # Select columns
  df <- df[,
           c(
             "country_code",  "country_name", "reporting_year",
             "surveyid_year", "survey_year", "survey_acronym",
             "survey_conductor", "survey_coverage",
             "welfare_type", "distribution_type",
             "survey_title", "year_start", "year_end",
             "authoring_entity_name", "abstract",
             "collection_dates_cycle", "collection_dates_start",
             "collection_dates_end",
             "sampling_procedure", "collection_mode",
             "coll_situation", "weight", "cleaning_operations"
           )
  ]

  # Create nested table

  df <- df[, .(.(.SD)),
           keyby =  .(
             country_code,
             country_name,
             reporting_year,
             survey_year,
             surveyid_year,
             survey_title,
             survey_conductor,
             survey_coverage,
             welfare_type,
             distribution_type
           )
  ]

  setnames(df, old = "V1", new = "metadata")

  ##  ............................................................................
  ##  Save                                                                    ####
  df <- df |> setnames("reporting_year", "year", skip_absent=TRUE)


  setattr(df, "aux_name", "metadata")
  key_cols <- c("country_code", "year", "welfare_type")
  setattr(df, "aux_key", key_cols)

  # validate raw metdata data
  metadata_validate_output(metadata = df, detail = detail)

  if (branch == "main") {
    branch <- ""
  }

  setattr(df, "gh", gh)


  saved <-  pip_aux_save(
    x        = df,
    id       = measure,
    pk       = key_cols,
    metadata = list(gh = gh),
    code     = aux_metadata_update,
    code_label = "aux_metadata_update"
  )

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(saved))

}

#' Metadata for PIP regions
#'
#' Update or load a dataset with regions.
#'
#' @inheritParams aux_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
aux_metaregion <- function(action = c("update", "load"),
                           owner   = getOption("pipfun.ghowner"),
                           verbose = FALSE,
                           tag     = NULL
) {

  measure <- "metaregion"
  action  <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    mr <- pipfun::load_from_gh(measure = measure,
                   owner    = owner,
                   branch   = branch,
                   ext = "csv")

    gh <- attributes(mr)$gh


    ##  ............................................................................
    ##  Save data                                                               ####

    if (branch == "main") {
      branch <- ""
    }

    key_cols <- c("region_code")
    setattr(mr, "aux_name", "metaregion")
    setattr(mr, "aux_key", key_cols)

    saved <-  pip_aux_save(
      x        = mr,
      id       = measure,
      pk       = key_cols,
      metadata = list(gh = gh),
      code     = aux_metaregion,
      code_label = "aux_metaregion"
    )

    return(invisible(saved))


  } else {

    df <- pipload::load_aux_data(measure = measure, verbose = verbose)

    return(df)
  }

} # end of function

#' Validate raw metadata data
#'
#' @param metadata raw metadata data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
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

#' Validate output metadata data
#'
#' @param metadata metadata data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
metadata_validate_output <- function(metadata, detail = getOption("pipaux.detail.output")){

  stopifnot("Metadata data is not loaded" = !is.null(metadata))

  report <- data_validation_report()

  validate(metadata, name = "Metadata output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.character(country_name),
                description = "`country_name` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.character(survey_title),
                description = "`survey_title` should be character") |>
    validate_if(is.character(survey_conductor),
                description = "`survey_conductor` should be character") |>
    validate_if(is.character(survey_coverage),
                description = "`survey_coverage` should be character") |>
    # TO FIX
    # validate_cols(in_set(c("national", "rural", "urban")),
    #               survey_coverage, description = "`survey_coverage` values within range") |>
    validate_if(is.character(welfare_type),
                description = "`welfare_type` should be character") |>
    validate_cols(in_set(c("consumption", "income")),
                  welfare_type, description = "`welfare_type` values within range") |>
    validate_if(is.character(distribution_type),
                description = "`distribution_type` should be character") |>
    validate_cols(in_set(c("aggregated", "group", "micro", "micro, imputed", NA)),
                  distribution_type, description = "`distribution_type` values within range") |>
    validate_cols(not_na, country_code, year, welfare_type,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, welfare_type),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}





