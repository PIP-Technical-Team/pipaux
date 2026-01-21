#' Country Profiles
#'
#' Update a list with country profiles data
#'
#' @inheritParams aux_countries
#' @inheritParams pipfun::load_from_gh
#' @export
aux_cp <- function(action  = c("update", "load"),
                   force   = FALSE,
                   owner   = getOption("pipfun.ghowner"),
                   tag     = NULL,
                  ...) {
  measure <- "cp"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    aux_cp_update(force   = force,
                  owner   = owner,
                  branch  = branch,
                  tag     = tag,
                ...)
  } else {

    dl <- pipload::load_aux_data(measure = measure)

    return(dl)
  }
}

aux_cp_clean <- function(x,
                         file_names) {

  #   ____________________________________________________________________
  #   Computations                                                    ####

  ## cleanup names -----------

  dl <- purrr::map(x, clean_cp_names)
  names(dl) <- gsub("(indicator_values_country_)(.*)", "\\2", file_names)

  ## Key Indicators ----------

  key_indicators <- merge(
    dl$KI1,
    dl$KI5_KI6_KI7,
    all = TRUE,
    by = c("country_code", "reporting_year")
  )

  key_indicators <- merge(
    x = key_indicators,
    y = dl$chart5[, c(
      "country_code",
      "reporting_year",
      "mpm_headcount",
      "ppp_year"
    )],
    all = TRUE,
    by = c("country_code", "reporting_year")
  )

  key_indicators <- list(
    headcount_national = key_indicators[, c(
      "country_code",
      "reporting_year",
      "headcount_national",
      "ppp_year"
    )],
    mpm_headcount = key_indicators[, c(
      "country_code",
      "reporting_year",
      "mpm_headcount",
      "ppp_year"
    )],
    reporting_pop = key_indicators[, c(
      "country_code",
      "reporting_year",
      "reporting_pop"
    )],
    gni = key_indicators[, c(
      "country_code",
      "reporting_year",
      "gni"
    )],
    gdp_growth = key_indicators[, c(
      "country_code",
      "reporting_year",
      "gdp_growth"
    )]
  )

  kg1 <- c("headcount_national", "mpm_headcount")
  for (i in seq_along(kg1)) {

    var <- kg1[i]

    key_indicators[[var]] <-
      key_indicators[[var]][
        !is.na(get(var)) & !is.na(ppp_year)
      ][,
        .SD[which.max(reporting_year)],
        by = c("country_code", "ppp_year")
      ]
  }

  key_indicators$reporting_pop <-
    key_indicators[["reporting_pop"]][
      !is.na(reporting_pop)
    ][,
      .SD[which.max(reporting_year)],
      by = c("country_code")
    ]

  ## ---- FIXED SECTION (warning removal, same output) ----

  key_indicators[4:5] <- lapply(key_indicators[4:5], function(x) {

  val_col <- setdiff(names(x), c("country_code", "reporting_year"))

  x %>%
    dplyr::filter(!is.na(.data[[val_col]])) %>%   # ✅ fixed
    dplyr::group_by(country_code) %>%
    dplyr::slice_tail(n = 2) %>%
    dplyr::mutate(
      latest = reporting_year == max(reporting_year)
    ) %>%
    dplyr::ungroup() %>%
    data.table::as.data.table()
})

  ## Additional charts ----------

  ab <- joyn::merge(
    x = dl$chart1_chart2_KI2_data,
    y = dl$chart1_chart2_KI2_ID,
    by = "id",
    match_type = "m:1",
    reportvar = FALSE,
    verbose = FALSE
  )
  ab[, id := NULL]

  dl$chart1_chart2_KI2      <- ab
  dl$chart1_chart2_KI2_data <- NULL
  dl$chart1_chart2_KI2_ID   <- NULL
  rm(ab)

  ## chart6 ------------

  ki4 <- dl$chart6_KI4[, c(
    "country_code",
    "year_range",
    "distribution",
    "shared_prosperity",
    "ppp_year"
  )]

  ki4$year1 <- sapply(strsplit(ki4$year_range, "-"), \(x) x[[1]])
  ki4$year2 <- sapply(strsplit(ki4$year_range, "-"), \(x) x[[2]])

  ki4 <- ki4 %>%
    dplyr::group_by(country_code, ppp_year) %>%
    dplyr::filter(distribution %in% c("b40", "tot")) %>%
    dplyr::filter(year2 == max(year2)) %>%
    dplyr::filter(year1 == max(year1)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      country_code,
      year_range,
      distribution,
      shared_prosperity,
      ppp_year
    ) %>%
    data.table::as.data.table() %>%
    data.table::dcast(
      country_code + ppp_year + year_range ~ distribution,
      value.var = "shared_prosperity"
    )

  setnames(
    ki4,
    old = c("b40", "tot"),
    new = c("share_below_40", "share_total")
  )

  key_indicators <- append(key_indicators, list(shared_prosperity = ki4))

  ## Charts ----------

  charts <- list(
    ineq_trend =
      dl$chart3[, c(
        "country_code",
        "reporting_year",
        "survey_acronym",
        "welfare_type",
        "survey_comparability",
        "comparable_spell",
        "gini",
        "theil",
        "reporting_level",
        "ppp_year"
      )],
    ineq_bar =
      dl$chart4[, c(
        "country_code",
        "reporting_year",
        "welfare_type",
        "survey_coverage",
        "gender",
        "agegroup",
        "education",
        "distribution",
        "poverty_share_by_group",
        "reporting_level",
        "ppp_year"
      )][,
        agegroup_label := fcase(
          agegroup == "0-14", "0 to 14 years old",
          agegroup == "15-64", "15 to 64 years old",
          agegroup == ">65", "65 and older",
          default = ""
        )
      ][,
        `:=`(
          education_label = education,
          gender_label    = gender
        )
      ],
    mpm =
      dl$chart5[, c(
        "country_code",
        "reporting_year",
        "welfare_type",
        "mpm_education_attainment",
        "mpm_education_enrollment",
        "mpm_electricity",
        "mpm_sanitation",
        "mpm_water",
        "mpm_monetary",
        "mpm_headcount",
        "ppp_year",
        "reporting_level"
      )],
    sp =
      dl$chart6_KI4[, c(
        "country_code",
        "year_range",
        "welfare_type",
        "distribution",
        "shared_prosperity",
        "ppp_year",
        "reporting_level"
      )]
  )

  cp <- list(
    key_indicators = key_indicators,
    charts = charts
  )

  return(cp)
}




#' Clean names from original CP files
#'
#' @param x data.table
#'
#' @return data.table with names clenad
clean_cp_names <- function(x) {

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  names(x) <- tolower(names(x))
  names(x) <- tolower(sub("xyzd[mcp]xyz", "", names(x)))

  # rename variables
  x <- setnames(
    x,
    skip_absent = TRUE,
    c(
      "country", "requestyear", "datayear", "welfaretype",
      "coverage", "interpolation", "survname", "comparability",
      "comparable_spell", "povertyline", "yearrange", "si_pov_all_poor",
      "sp_pop_totl", "si_pov_nahc", "ny_gnp_pcap_cd", "ny_gdp_mktp_kd_zg",
      "si_pov_gini", "si_pov_theil", "si_pov_all", "si_pov_share_all",
      "si_mpm_educ", "si_mpm_edue", "si_mpm_elec", "si_mpm_imps",
      "si_mpm_impw", "si_mpm_mdhc", "si_mpm_poor", "si_spr_pcap_zg", "pppyear"
    ),
    c(
      "country_code", "reporting_year", "survey_year", "welfare_type",
      "survey_coverage", "is_interpolated", "survey_acronym",
      "survey_comparability", "comparable_spell",
      "poverty_line", "year_range", "pop_in_poverty",
      "reporting_pop", "headcount_national", "gni", "gdp_growth",
      "gini", "theil", "headcount", "poverty_share_by_group",
      "mpm_education_attainment", "mpm_education_enrollment",
      "mpm_electricity", "mpm_sanitation", "mpm_water",
      "mpm_headcount", "mpm_monetary", "shared_prosperity", "ppp_year"
    )
  )

  if (any(grepl("welfare_type", names(x)))) {

    x[,
      welfare_type := fifelse(welfare_type == "CONS",
                              "consumption", "income")]
  }

  if (any(grepl("survey_coverage", names(x)))) {
    # Recode survey coverage
    x[,
      `:=`(survey_coverage = fcase(
        survey_coverage == "N", "national",
        survey_coverage == "R", "rural",
        survey_coverage == "U", "urban",
        default = ""
      )
      )]

    x[,
      reporting_level := ifelse(survey_coverage == "",
                                "national",
                                survey_coverage)
    ]
  }

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(x)

}

#' Update Country Profiles
#'
#' Update a list with country profiles data
#'
#' @inheritParams aux_cp
#' @keywords internal
aux_cp_update <- function(force = FALSE,
                          owner   = getOption("pipfun.ghowner"),
                          branch,
                          tag     = tag,
                        ...) {

  measure <- "cp"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## chart files --------

  file_names <-
    c(
      "indicator_values_country_chart4",
      "indicator_values_country_KI1",
      "indicator_values_country_chart1_chart2_KI2_data",
      "indicator_values_country_chart1_chart2_KI2_ID",
      "indicator_values_country_chart5",
      "indicator_values_country_chart3",
      "indicator_values_country_chart6_KI4",
      "indicator_values_country_KI5_KI6_KI7"
    )


  raw_files <- purrr::map(.x = file_names,
                          .f = ~{
                            pipfun::load_from_gh(
                              measure = "cp",
                              owner  = owner,
                              branch = branch,
                              filename = .x,
                              ext = "csv")
                          })
  # Collect gh attributes from all raw files
  gh_list <- lapply(raw_files, function(x) attributes(x)$gh)
  gh_list <- gh_list[!vapply(gh_list, is.null, logical(1))] # remove NULLs


  dl <- aux_cp_clean(raw_files,
                     file_names)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## download files --------
  fl_files <- c("flat_cp", "flat_shp")

  raw_fl <- purrr::map(.x = fl_files,
                       .f = ~{
                         x <- pipfun::load_from_gh(
                           measure = "cp",
                           owner  = owner,
                           branch = branch,
                           filename = .x,
                           ext = "dta")
                         setnames(x, "year", "reporting_year",
                                  skip_absent=TRUE)
                       })
  names(raw_fl) <- fl_files
  dl <- append(dl, list(flat = raw_fl))



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## save --------

  if (branch == "main") {
    branch <- ""
  }

  # ----- function raw sha ----------------------

  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )


  setattr(dl,
          "raw_sha_fun",
          raw_sha_fun)


  # Define key columns for country profiles data
  key_cols <- names(dl)
  setattr(dl, "aux_key", key_cols)
  
  saved <- pip_aux_save(
    x        = dl,
    id       = measure,
    force    = force,
    metadata = list(gh = gh_list),
    code     = aux_cp_update,
    code_label = "aux_cp_update",
    #pk       = key_cols, removed because of list
    ...
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## return  --------

  return(invisible(saved))
}

