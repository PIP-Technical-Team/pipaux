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
                             maindir      = getOption("pipaux.working_dir"),
                             force        = FALSE,
                             detail       = getOption("pipaux.detail.raw")
                             ) {
  measure <- "country_list"
  action  <- match.arg(action)

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  class_branch = "master"

  if (action == "update") {

    ## Special national accounts --------
    cl <- aux_country_list_update(class_branch = class_branch)

  # validate country list raw data
    cl_validate_raw(cl, detail = detail)

    # Save
    if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

  # ----- function raw sha ------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )

  setattr(cl, "aux_name", "country_list")
  setattr(cl,
          "aux_key",
          c("country_code"))

  setattr(cl,
          "raw_sha_fun",
          raw_sha_fun)

    saved <- pipfun::pip_sign_save(
      x       = cl,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(invisible(saved))

  } else {

    df <- load_aux(maindir = maindir,
                   measure = measure,
                   branch  = branch)
    return(df)
  }
}

#' Update Country LIst
#'
#' @param class_branch character: names of branch of GPID-WB/class repo. Default
#'   if master
aux_country_list_update <-
  function(class_branch = "master") {

    # Check arguments
    measure <- "country_list"

    #   ____________________________________________________________________________
    #   Read Data from WDI                                                      ####

    wdi <-
      wbstats::wb_countries()  |>
      as.data.table() |>
      {
        \(.) {

          # clean data

          iso2 <- grep("_iso2c", names(.), value = TRUE)
          x    <- .[, !..iso2]

          iso3 <- grep("_iso3c", names(x), value = TRUE)

          withiso <-
            gsub("_iso3c", "", iso3) |>
            paste0(collapse = "|") |>
            grep(names(x), value = TRUE)

          tokeep <- c("country", "iso3c", withiso)

          x[region != "Aggregates"
          ][,
            ..tokeep
          ]
        }
      }()


    # rename iso3c
    owdi <- names(wdi)
    nwdi <-
      gsub("iso3c", "code", names(wdi))

    setnames(wdi, owdi, nwdi)


    # Add "(excluding high income)" to South Asia
    wdi[, admin_region  := fifelse(test = grepl("income", admin_region) | is.na(admin_region),
                                   yes  = admin_region ,
                                   no   = paste(admin_region , "(excluding high income)"))]

    #   ____________________________________________________________________________
    #   Read data from CLASS.dta file                                           ####

    ## Special national accounts --------
    byv <-
      c(
        "code",
        "region_SSA",
        "fcv_current",
        "region_pip")

    dt <- pipfun::load_from_gh(
      measure  = measure,
      owner    = "GPID-WB",
      repo     = "Class",
      branch   = class_branch,
      filename = "OutputData/CLASS",
      ext      = "dta"
    ) |>
      as.data.table() |>
      unique(by = byv) |>
      (\(.){.[, ..byv]})()  # select just these variables


    dt_o <- names(dt)
    dt_n <- gsub("_current", "", dt_o)

    setnames(dt, dt_o, dt_n)
    setnames(dt,
             old = c("region_SSA", "region_pip"),
             new = c("africa_split_code", "pip_region_code"))

    #   ____________________________________________________________________________
    #   Merge wdi and CLASS                                                     ####


    rg <-
      joyn::joyn(dt, wdi,
                 by = "code",
                 match_type = "1:1",
                 reportvar = FALSE,
                 verbose =  FALSE)


    #   ____________________________________________________________________________
    #   Clean Data                                                              ####

    # PIP region

    rg[, pip_region := fifelse(pip_region_code == "OHI",
                               yes = "Other High Income Countries",
                               no  = region)
    ]



    # East and West Africa

    rg[,
       africa_split :=  fcase(
         africa_split_code == "", "",
         africa_split_code == "AFE", "Eastern and Southern Africa",
         africa_split_code == "AFW", "Western and Central Africa",
         default = "")
    ][,
      africa_split_code := fifelse(test = africa_split_code == "",
                                   yes  = "",
                                   no   =   africa_split_code)
    ]

    # Fragile countries

    rg[,
       fcv_code := fifelse(fcv == "Yes", "FCVT", "FCVF")
    ][,
      fcv := fifelse(fcv == "Yes", "Fragile", "Not-fragile")]

    ## Admin regions

    rg[,
       admin_region_code := fifelse(
         admin_region_code == "" | is.na(admin_region_code),
         NA_character_,
         paste0(admin_region_code, "-AD"))]


    # Add PCN region temporarilly

    rg[,
       `:=`(
         pcn_region = pip_region,
         pcn_region_code = pip_region_code
       )]

    # ff <- copy(rg)
    # rg <- copy(ff)

    # Convert empty strings to NA
    vars <- names(rg)
    names(vars) <- vars
    rg[, (vars) := lapply(.SD,
                          \(x) {
                            fifelse(x == "" | is.na(x), NA_character_, x)
                          }
    )
    ]


    # fix "Not classified"
    # ff <- copy(rg)

    # rg <- copy(ff)

    # not_class <- function(x) {
    #   y <- deparse(substitute(x))
    #   fifelse(test = grepl("classified", x),
    #           paste(x, "by",  y),
    #           x)
    # }
    #
    # rg[, (vars) := lapply(.SD,not_class), .SDcols = vars]
    #
    #
    # rg[, (vars) := lapply(.SD,
    #                       \(x){
    #                         y <- deparse(substitute(x))
    #                         # y <- ..x
    #                         fifelse(test = grepl("classified", x),
    #                                 paste(x, "by",  y),
    #                                 x)
    #                         })]
    #
    #
    # rg[lending_type_code == "LNX", unique(lending_type)]
    #



    rg[, lending_type := fifelse(grepl("classified", lending_type),
                                 paste(lending_type, "by", "lending type"),
                                 lending_type)]



    rg[, income_level := fifelse(grepl("classified", income_level),
                                 paste(income_level, "by", "income level"),
                                 income_level)]


    # Create the World

    rg[, `:=`(
      world      = "World",
      world_code =  "WLD"
    )]



    # janitor::tabyl(rg, region_code, admin_region_code)
    # janitor::tabyl(rg, region, admin_region)
    # janitor::tabyl(rg, region, pip_region)

    #   ____________________________________________________________________________
    #   Clean and Save                                                    ####


    rg[,
       c( "region_code", "region") := NULL]


    setnames(x = rg,
             old = c("code", "country", "pip_region", "pip_region_code"),
             new = c("country_code", "country_name", "region", "region_code") )



    ## Order columns alphabetically ------------
    varn <- names(rg)
    setcolorder(rg, sort(varn))
    setcolorder(rg, c("country_code", "country_name"))


    ## Remove  categoeries that we don't need ---------

    rm_agg <- c("fcv",  "lending_type", "admin_region")
    rm_agg <- c("fcv", "income_level", "lending_type", "admin_region")

    to_rm <-
      rm_agg |>
      paste0("_code") |>
      c(rm_agg)

    rg[, (to_rm) := NULL]


    # hardcode fixing of TWN's name
    rg[country_code == "TWN",
       country_name  := "Taiwan, China"]


    rg

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

  # country_list <- pipload::pip_load_aux("pfw")
  country_list <- pipfun::load_from_gh(measure = "pfw",
                                       owner   = getOption("pipfun.ghowner"),
                                       branch  = "DEV",
                                       ext = "dta")

  country_list <- unique(country_list[, code])

  validate(cl, name = "CL raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    # validate_cols(in_set(country_list),
    #               country_code, description = "`country_code` values within range") |>
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
    validate_if(is.character(pcn_region),
                description = "`pcn_region` should be character") |>
    validate_if(is.character(pcn_region_code),
                description = "`pcn_region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  pcn_region_code, description = "`pcn_region_code` values within range") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    validate_if(is.character(region_code),
                description = "`region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
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


