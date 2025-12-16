#' List of countries
#'
#' Load or update dataset with WDI countries. See details.
#'
#' This function creates a combined dataset of countries in WDI and their
#' respective regional classification by querying `wbstats::wb_countries()`, as
#' well as reading from the PovcalNet Masterfile to fetch PCN region codes.
#'
#' The dependency on the PCN Masterfile should be changed in the future.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_countries
#' @inheritParams pipfun::load_from_gh
#' @export
#' @return logical if `action = "update"` or data.table if `action = "load"`
aux_country_list <- function(action       = c("update", "load"),
                             force        = FALSE,
                             detail       = getOption("pipaux.detail.raw")
                             ) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####

  measure <- "country_list"
  action  <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  class_branch = "master"

  if (action == "update") {

    ## Special national accounts --------
    cl <- aux_country_list_update(class_branch = class_branch)

    #validate country list raw data
    cl_validate_raw(cl, detail = detail)


    #   ____________________________________________________________________________
    #   Metadata        -stored under $user in pin metadata                                                       ####

    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )

    key_cols <- c("country_code")

    # cl_metadata <- list(raw_sha_fun = raw_sha_fun,
    #                     key_col     = key_cols)

    setattr(cl,
            "aux_key",
            key_cols)

    setattr(cl,
            "raw_sha_fun",
            raw_sha_fun)



    saved <- pip_aux_save(
      x        = cl,
      id       = measure,
      force    = force,
      pk       = key_cols
    )

    return(invisible(saved))

  } else {

    df <- pipload::load_aux_data(measure = measure)

    return(df)
  }
}

#' Update Country LIst
#'
#' @param class_branch character: names of branch of GPID-WB/class repo. Default
#'   if master
#' @keywords internal
aux_country_list_update <-
  function(class_branch = "master") {

    # Check arguments
    measure <- "country_list"
    #   ____________________________________________________________________________
    #   Read data from CLASS.dta file                                           ####


    dt <- pipfun::load_from_gh(
      measure  = measure,
      owner    = "GPID-WB",
      repo     = "Class",
      branch   = class_branch,
      filename = "OutputData/CLASS",
      ext      = "dta"
    ) |>
      setDT()


    # collapse table to unique identifiers by country, NOT by country/year
    rm_names <- grep("year|historical", names(dt), value = TRUE)

    dt <- dt[,
             # max year per country to get current classifications
             .SD[year_data == fmax(year_data)], by = code
    ][,
      # Remove year variables
      (rm_names) := NULL] |>
      funique()

    setnames(x = dt,
             old = c("code", "economy" ),
             new = c("country_code", "country_name") )


    dt[country_code == "SOM",
       country_name := "Federal Republic of Somalia"]

    # hardcode fixing of TWN's name
    dt[country_code == "TWN",
       country_name  := "Taiwan, China"]




    ssa_old <- grep("regionssa", names(dt), value = TRUE)
    ssa_news <- gsub("regionssa", "africa_split", ssa_old)


    setnames(dt,
             old = ssa_old,
             new = ssa_news)

    # Fragile countries
    dt[,
       fcv := fifelse(fcv == "Yes", "Fragile", "Not-fragile")]


    # Convert empty strings to NA
    recode_char(dt, `^$` = NA_character_,  regex = TRUE, set = TRUE)

    # Create the World

    dt[, `:=`(
      world      = "World",
      world_code =  "WLD"
    )]



    ## Order columns alphabetically ------------
    varn <- names(dt)
    setcolorder(dt, sort(varn))
    setcolorder(dt, c("country_code", "country_name",
                      "region", "region_code"))



    dt

    ## order data to get the right unicode
    setorder(dt, country_code)

    priority <- c("CIV","STP","CUW","TUR")

    wp  <- which( dt$country_code %in% priority)
    wnp <- which(!dt$country_code %in% priority)

    new_order <- c(wp, wnp)

    dt[new_order]

  }

#' Validate raw country list data
#'
#' @param cl raw country list data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
cl_validate_raw <- function(cl, detail = getOption("pipaux.detail.raw")){

  stopifnot("Country list raw data is not loaded" = !is.null(cl))

  report <- data_validation_report()

  validate(cl, name = "CL raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.character(country_name),
                description = "`country_name` should be character") |>
    validate_if(is.character(africa_split),
                description = "`africa_split` should be character") |>
    validate_cols(in_set(c("Eastern and Southern Africa", "Western and Central Africa", NA)),
                  africa_split, description = "`africa_split` values within range") |>
    validate_if(is.character(africa_split_code),
                description = "`africa_split_code` should be character") |>
    validate_cols(in_set(c("AFE", "AFW", NA)),
                  africa_split_code, description = "`africa_split_code` values within range") |>
    validate_if(is.character(regionpcn),
                description = "`regionpcn
                ` should be character") |>
    validate_if(is.character(regionpcn_code),
                description = "`regionpcn_code` should be character") |>
    validate_cols(in_set(c("SSA", "OHI", "SAS", "ECA", "LAC", "EAP", "MNA")),
                  regionpcn_code, description = "`regionpcn_code` values within range") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    validate_if(is.character(region_code),
                description = "`region_code` should be character") |>
    validate_cols(in_set(c( "SSF", "LCN", "MEA", "ECS", "EAS", "SAS", "NAC")),
                  region_code, description = "`region_code` values within range") |>
    validate_if(is.character(world),
                description = "`world` should be character") |>
    validate_cols(in_set(c("World")),
                  world, description = "`world` values within range") |>
    validate_if(is.character(world_code),
                description = "`world_code` should be character") |>
    validate_cols(in_set(c("WLD")),
                  world_code, description = "`world_code` values within range") |>
    validate_cols(not_na, country_code,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}


