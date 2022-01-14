#' Transform CPI
#'
#' Clean CPI data from Datalibweb to meet PIP protocols.
#'
#' @param df data.frame: CPI data.
#' @param id character: CPI ID.
#' @param cpi_var character: CPI variable to be used as default.
#'
#' @return data.table
#' @keywords internal
transform_cpi <- function(df, id, cpi_var) {

  # Convert to data.table
  dt <- data.table::as.data.table(df)

  # modifications to the database
  dt[
    ,
    c("cur_adj", "ccf")
    := {
      cur_adj <- ifelse(is.na(cur_adj), 1, cur_adj)
      ccf <- 1 / cur_adj
      list(cur_adj, ccf)
    }
  ][
    ,
    `:=`(
      country_code   = code,
      cpi_year       = as.integer(year),
      survey_year    = ref_year,
      cpi            = get(cpi_var),
      survey_acronym = survname,
      cpi_id         = (id),
      cpi_domain     = as.character(cpi_domain),
      cpi_data_level = as.character(cpi_data_level)
    )
  ][
    ,
    # This part should not exist if the raw data
    # had been created properly
    cpi_data_level := fcase(
      cpi_domain %chin% c("urban/rural", "2") & cpi_data_level == "0", "rural",
      cpi_domain %chin% c("urban/rural", "2") & cpi_data_level == "1", "urban",
      cpi_domain %chin% c("national", "1") & cpi_data_level %chin% c("2", "", NA_character_),
      "national",
      default = ""
    )
  ]

  # Select columns
  keep_vars <- c(
    "country_code", "cpi_year", "survey_year",
    "cpi", "ccf", "survey_acronym", "change_cpi2011",
    grep("^cpi.+", names(df), value = TRUE), "cpi_id")
  dt <- dt[, ..keep_vars]

  # Remove countries not in WDI?
  # ...

  #  Remove duplicates
  dt <- unique(dt)

  return(dt)
}


#' Verify input CPI
#'
#' @inheritParams transform_cpi
#' @return data.frame
#' @keywords internal
verify_input_cpi <- function(df){

  df %>%
    assertr::verify(
      assertr::has_all_names("code", "year", "ref_year", "survname",
                             "cur_adj", "cpi_domain", "cpi_data_level"))


  return(df)

}

#' Verify output CPI
#'
#' @inheritParams transform_cpi
#' @return data.table
#' @keywords internal
verify_output_cpi <- function(df){

  df %>%
    assertr::verify(
      assertr::has_only_names(
        'country_code', 'cpi_year', 'survey_year', 'cpi', 'ccf',
        'survey_acronym', 'change_cpi2011', 'cpi2011', 'cpi_domain',
        'cpi_domain_value', 'cpi2011_unadj', 'cpi_final_2019', 'cpi_data_level',
        'cpi2011_SM21', 'cpi2011_unadj_SM21', 'cpi2005_SM21', 'cpi2017',
        'cpi_id'))


  return(df)

}
