#' PIP wdi
#'
#' Update or load wdi data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_wdi <- function(action          = c("update", "load"),
                    owner           = getOption("pipfun.ghowner"),
                    tag             = NULL,
                    detail          = getOption("pipaux.detail.raw")) {

  measure    <- "wdi"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }


  if (action == "update") {
    aux_wdi_update(owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
}


#' Update National accounts data from WDI
#'
#' GDP and HFCE data from WDI.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @param branch character, branch in which to find the data, default is NULL, which means it will be determined based on the current release and identity
#' @inheritParams aux_gdp
#' @return data.table with gdp and pce variables
#' @export
#'
#' @examples
#' \dontrun{
#' aux_wdi_update()}
aux_wdi_update <- function(owner   = getOption("pipfun.ghowner"),
                           branch  = NULL,
                           tag     = branch,
                           detail  = getOption("pipaux.detail.raw")) {


  #   ______________________________________________________
  #   Computations                                    ####
  measure <- "wdi"

  ##  ...............................................................
  ##  From file                                          ####

    wdi <- pipfun::load_from_gh(measure = measure,
                  owner = owner,
                  branch = branch,
                  ext    = "csv")

    gh <- attributes(wdi)$gh


  # validate wdi raw data
  wdi_validate_raw(wdi = wdi, detail = detail)

  #   _________________________________________________________________________
  #   Save and Return                                                     ####

  if (branch == "main") {
    branch <- ""
  }


  setattr(wdi, "aux_name", "wdi")
  key_cols <- c("country_code", "year")
  setattr(wdi, "aux_key", key_cols)

  saved <-  pip_aux_save(
    x        = wdi,
    id       = measure,
    pk       = key_cols,
    metadata = list(gh = gh),
    code     = aux_wdi_update,
    code_label = "aux_wdi_update"
  )

  return(invisible(saved))

}

#' Validate raw wdi data
#'
#' @param wdi raw wdi data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
wdi_validate_raw <- function(wdi, detail = getOption("pipaux.detail.raw")){

  stopifnot("WDI raw data is not loaded" = !is.null(wdi))

  report <- data_validation_report()

  validate(wdi, name = "WDI raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(NE.CON.PRVT.PC.KD),
                description = "`NE.CON.PRVT.PC.KD` should be numeric") |>
    validate_if(is.numeric(NY.GDP.PCAP.KD),
                description = "`NY.GDP.PCAP.KD` should be numeric") |>
    validate_cols(not_na, country_code, year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}


