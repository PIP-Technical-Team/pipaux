#' PIP wdi
#'
#' Update or load wdi data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
aux_wdi <- function(action          = c("update", "load"),
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = getOption("pipfun.ghowner"),
                    tag             = match.arg(branch),
                    detail          = getOption("pipaux.detail.raw")) {

  measure    <- "wdi"
  action <- match.arg(action)

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)



  if (action == "update") {
    aux_wdi_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)

  } else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
} # end of pip_wdi

#' Update National accounts data from WDI
#'
#' GDP and HFCE data from WDI.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_gdp
#' @return data.table with gdp and pce variables
#' @export
#'
#' @examples
#' aux_wdi_update()
aux_wdi_update <- function(force   = FALSE,
                           maindir = gls$PIP_DATA_DIR,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = paste0(wrk_release$release, "_", wrk_release$identity),
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


  # validate wdi raw data
  wdi_validate_raw(wdi = wdi, detail = detail)

  #   _________________________________________________________________________
  #   Save and Return                                                     ####

  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

  setattr(wdi, "aux_name", "wdi")
  setattr(wdi,
          "aux_key",
          c("country_code", "year"))

  # ----- function raw sha -----------------------------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )


  setattr(wdi,
          "raw_sha_fun",
          raw_sha_fun)

  saved <- pipfun::pip_sign_save(
    x       = wdi,
    measure = measure,
    msrdir  = msrdir,
    force   = force,
    save_dta = FALSE
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


