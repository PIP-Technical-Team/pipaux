#' Poverty lines
#'
#' Update or load a dataset with poverty lines.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_pl <- function(action = c("update", "load"),
                   force = FALSE,
                   owner   = getOption("pipfun.ghowner"),
                   tag     = NULL,
                   detail  = getOption("pipaux.detail.raw")
                   ) {

  measure <- "pl"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }


  if (action == "update") {
    # Read yaml file

    dl <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "yaml"
    )

    gh <- lapply(dl, function(x) attributes(x)$gh)

    dt <- purrr::map_df(dl,aux_pl_clean)

  # Save

  # validate pl clean data
    pl_validate_output(pl = dt, detail = detail)

    if (branch == "main") {
      branch <- ""
    }

  # ----- function raw sha ----------------------

  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )

  setattr(dt,
          "raw_sha_fun",
          raw_sha_fun)


    key_cols <- c("poverty_line")
    setattr(dt, "aux_key", key_cols)
    saved <- pip_aux_save(
      x        = dt,
      id       = measure,
      pk       = key_cols,
      force    = force,
      metadata = list(gh = gh),
      code     = aux_pl,
      code_label = "aux_pl"
    )


    return(invisible(saved))

  } else {

    df <- pipload::load_aux_data(measure = measure)

    return(df)
  }
}

#' Build a data table for each list from yaml file with poverty lines info
#'
#' @param l list from yaml file
#'
#' @return data.table
#' @export
aux_pl_clean <- function(l) {


  #   ____________________________________________________________________________
  #   Computations                                                            ####

  pls <-
    purrr::map(.x = l$ranges,
               .f = ~{
                 seq(.x$min, .x$max, .x$increment)
               }) |>
    unlist()

  # Create data frame
  df <- data.table::data.table(
    name = as.character(pls),
    poverty_line = pls
  )


  df[,
     c("is_default", "is_visible", "name", "ppp_year")
     := {
       id   <- fifelse(name == l$default, TRUE, FALSE)

       iv   <- fifelse(name %in% l$visible, TRUE, FALSE)

       n <- fifelse(n_decimals(poverty_line) == 1, paste0(name, "0"), name)
       n <- fifelse(n_decimals(poverty_line) == 0, paste0(n, ".00"), n)

       list(id, iv, n, l$ppp_year)
     }]

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(df)

}

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


