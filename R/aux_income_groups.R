#' PIP series of income group
#'
#' Update or load a dataset with historical income groups. The raw files are not
#' available in the PIP-Technical-Team group but in the Povcalnet-team group.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
aux_income_groups <- function(action       = c("update", "load"),
                              #force        = FALSE,
                              owner        = getOption("pipfun.ghowner"),
                              detail       = getOption("pipaux.detail.raw")
) {

  measure <- "income_groups"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  class_branch <- "master"

  if (action == "update") {

    ## Special national accounts --------
    ig <- pipfun::load_from_gh(
      measure  = measure,
      owner    = "GPID-WB",
      repo     = "Class",
      branch   = class_branch,
      filename = "OutputData/CLASS",
      ext      = "dta"
    ) |>
      get_vars(c('code',
                 'year_data',
                 'incgroup',
                 'fcv',
                 'regionssa')) |>
      # create variables for future development
      ftransform(year         = year_data,
                 income_group = incgroup)

    ig[,
       income_group_code := fcase(income_group == "High income", "HIC",
                                  income_group == "Upper middle income", "UMIC",
                                  income_group == "Lower middle income", "LMIC",
                                  income_group == "Low income", "LIC",
                                  default = "")]
    setnames(ig,
             c("code", "regionssa"),
             c("country_code", "ssa_subregion_code"))

    ### Get info

    # ----- file raw sha ------

    gh <- attr(ig,
               "gh")

    # ----- function raw sha ------
    raw_sha_fun <- digest::digest(body(
      aux_income_groups)
      )


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## save --------
    # ig <- ig |> setnames("year_data", "year", skip_absent=TRUE)

    setattr(ig, "aux_name", "income_groups")

      key_cols <- c("country_code", "year")
      setattr(ig, "aux_key", key_cols)

    setattr(ig,
           "raw_sha_fun",
           raw_sha_fun)

    # validate income group output data
    incgroup_validate_output(incgroup = ig, detail = detail)

    if (branch == "main") {
      branch <- ""
    }


    saved <-  pip_aux_save(
      x        = ig,
      id       = measure,
      pk       = key_cols,
      metadata = list(gh = gh),
      code     = aux_income_groups,
      code_label = "aux_income_groups"
    )

    return(invisible(saved))

  } else  {

    pipload::load_aux_data(measure = measure)

  }
}

#' Validate income group output data
#'
#' @param incgroup income group output data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
incgroup_validate_output <- function(incgroup, detail = getOption("pipaux.detail.output")){

  stopifnot("Income group output data is not loaded" = !is.null(incgroup))

  report <- data_validation_report()

  validate(incgroup, name = "Income group output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year_data),
                description = "`year_data` should be numeric") |>
    validate_if(is.character(income_group),
                description = "`income_group` should be character") |>
    validate_cols(in_set(c("High income", "Low income", "Lower middle income", "Upper middle income")),
                  income_group, description = "`income_group` values within range") |>
    validate_if(is.character(income_group_code),
                description = "`income_group_code` should be character") |>
    validate_cols(in_set(c("HIC", "LIC", "LMIC", "UMIC")),
                  income_group_code, description = "`income_group_code` values within range") |>
    validate_if(is.character(incgroup),
                description = "`incgroup_historical` should be character") |>
    validate_cols(in_set(c("High income", "Low income", "Lower middle income", "Upper middle income")),
                  incgroup, description = "`incgroup` values within range") |>
    validate_if(is.character(fcv),
                description = "`fcv` should be character") |>
    validate_if(is.character(ssa_subregion_code),
                description = "`ssa_subregion_code` should be character") |>
    validate_cols(not_na, country_code, year_data,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year_data),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

