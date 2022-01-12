#' Load raw Country Profiles
#'
#' @param dir character: Measurement directory
#' @param pattern character: File pattern
#' @return list
#' @keywords internal
load_country_profiles <- function(dir, pattern = "indicator_values_country"){

  files <- list.files(dir, pattern, full.names = TRUE)
  dl <- lapply(files, read.csv)
  return(dl)

}

#' Transform Country Profiles
#'
#' @param dl list: Country Profiles object.
#' @return list
#' @keywords internal
transform_country_profiles <- function(dl){

  dl <- lapply(dl, function(x) {
    x <- data.table::setDT(x)
    names(x) <- tolower(sub("xyzD[MCP]xyz", "", names(x)))
    x <- data.table::setnames(
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
      x$welfare_type <- data.table::fifelse(
        x$welfare_type == "CONS", "consumption", "income"
      )
    }
    if (any(grepl("survey_coverage", names(x)))) {
      # Recode survey coverage
      x <- x[, `:=`(
        survey_coverage = data.table::fcase(
          survey_coverage == "N", "national",
          survey_coverage == "R", "rural",
          survey_coverage == "U", "urban",
          default = ""
        )
      )]
    }
    return(x)
  })
  names(dl) <- gsub(".*indicator_values_country_|[.]csv", "", files)

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
    headcount_national = key_indicators[, c("country_code", "reporting_year", "headcount_national")],
    mpm_headcount = key_indicators[, c("country_code", "reporting_year", "mpm_headcount")],
    reporting_pop = key_indicators[, c("country_code", "reporting_year", "reporting_pop")],
    gni = key_indicators[, c("country_code", "reporting_year", "gni")],
    gdp_growth = key_indicators[, c("country_code", "reporting_year", "gdp_growth")]
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
        "gini", "theil"
      )],
    ineq_bar =
      dl$chart4[, c(
        "country_code", "reporting_year", "welfare_type",
        "survey_coverage", "gender", "agegroup", "education",
        "distribution", "poverty_share_by_group"
      )],
    mpm =
      dl$chart5[, c(
        "country_code", "reporting_year",
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

  return(cp)

}



#' Verify input Country Profiles
#'
#' @inheritParams transform_country_profiles
#' @return list
#' @keywords internal
verify_input_country_profiles <- function(dl){

}

#' Verify output Country Profiles
#'
#' @inheritParams transform_country_profiles
#' @return list
#' @keywords internal
verify_output_country_profiles <- function(dl){

}
