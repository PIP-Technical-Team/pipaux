#' Clean country profile data
#'
#' @param x database from pip_cp_update
#' @param file_names character: vector with names of files
#'
#' @return data.table
pip_cp_clean <- function(x,
                         file_names) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####

  dl <- purrr::map(x, clean_cp_names)
  names(dl) <- gsub("(indicator_values_country_)(.*)", "\\2", file_names)

  # Create list of key indicators datasets
  key_indicators <- merge(dl$KI1, dl$KI5_KI6_KI7,
                          all = TRUE,
                          by = c("country_code", "reporting_year")
  )

  key_indicators <- merge(key_indicators,
                          dl$chart5[, c("country_code", "reporting_year", "mpm_headcount")],
                          all = TRUE,
                          by = c("country_code", "reporting_year")
  )

  key_indicators <- list(
    headcount_national = key_indicators[,
                                        c("country_code",
                                          "reporting_year",
                                          "headcount_national")],
    mpm_headcount = key_indicators[,
                                   c("country_code",
                                     "reporting_year",
                                     "mpm_headcount")],

    reporting_pop = key_indicators[,
                                   c("country_code",
                                     "reporting_year",
                                     "reporting_pop")],

    gni = key_indicators[, c("country_code",
                             "reporting_year",
                             "gni")],

    gdp_growth = key_indicators[, c("country_code",
                                    "reporting_year",
                                    "gdp_growth")]
  )

  key_indicators[1:3] <- lapply(key_indicators[1:3], function(x) {
    x <- x %>%
      dplyr::filter(!is.na(x[, 3])) %>%
      dplyr::group_by(country_code) %>%
      dplyr::filter(reporting_year == max(reporting_year)) %>%
      dplyr::ungroup() %>%
      data.table::as.data.table()
  })

  key_indicators[4:5] <- lapply(key_indicators[4:5], function(x) {
    x <- x %>%
      dplyr::filter(!is.na(x[, 3])) %>%
      dplyr::group_by(country_code) %>%
      dplyr::slice_tail(n = 2) %>%
      dplyr::mutate(
        latest =
          dplyr::if_else(reporting_year == max(reporting_year),
                         TRUE, FALSE
          )
      ) %>%
      dplyr::ungroup() %>%
      data.table::as.data.table()
  })

  ki4 <- dl$chart6_KI4[, c(
    "country_code", "year_range",
    "distribution", "shared_prosperity"
  )]

  ki4$year1 <- sapply(strsplit(ki4$year_range, "-"), function(x) x[[1]])
  ki4$year2 <- sapply(strsplit(ki4$year_range, "-"), function(x) x[[2]])
  ki4 <- ki4 %>%
    dplyr::group_by(country_code) %>%
    dplyr::filter(distribution %in% c("b40", "tot")) %>%
    dplyr::filter(year2 == max(year2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      country_code, year_range,
      distribution, shared_prosperity
    ) %>%
    data.table::as.data.table() %>%
    data.table::dcast(country_code + year_range ~ distribution,
                      value.var = "shared_prosperity"
    )

  names(ki4)[3:4] <- c("share_below_40", "share_total")
  key_indicators <- append(key_indicators, list(shared_prosperity = ki4))

  # Create list of charts datasets
  mpm_cols <- grep("mpm_", names(dl$chart5), value = TRUE)

  charts <- list(
    ineq_trend =
      dl$chart3[, c(
        "country_code", "reporting_year",
        "survey_acronym", "welfare_type",
        "survey_comparability", "comparable_spell",
        "gini", "theil", "reporting_level"
      )],
    ineq_bar =
      dl$chart4[, c(
        "country_code", "reporting_year", "welfare_type",
        "survey_coverage", "gender", "agegroup", "education",
        "distribution", "poverty_share_by_group", "reporting_level"
      )][,
         agegroup_label := fcase(
           agegroup == "0-14", "0 to 14 years old",
           agegroup == "15-64", "15 to 64 years old",
           agegroup == ">65", "65 and older",
           default = ""
         )][,
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
        "mpm_headcount"
      )],
    sp =
      dl$chart6_KI4[, c(
        "country_code", "year_range",
        "welfare_type", "distribution",
        "shared_prosperity"
      )]
  )

  cp <- list(key_indicators = key_indicators, charts = charts)
}





#' Clean names from original CP files
#'
#' @param x data.table
#'
#' @return data.table with names clenad
clean_cp_names <- function(x) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  names(x) <- tolower(sub("xyzD[MCP]xyz", "", names(x)))

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
      "si_mpm_impw", "si_mpm_mdhc", "si_mpm_poor", "si_spr_pcap_zg"
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
      "mpm_headcount", "mpm_monetary", "shared_prosperity"
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
