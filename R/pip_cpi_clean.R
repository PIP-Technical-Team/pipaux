#' Clean CPI data from datalibweb to meet PIP protocols
#'
#' @param y dataset with CPI data from datalibweb. loaded in `pip_prices()`.
#' @param cpivar character: CPI variable to be used as default. Currently it is
#' "cpi2011".
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_clean <- function(y, cpivar = getOption("pipaux.cpivar")) {

  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c("country_code", "surveyid_year", "reference_year",
                 "cpi", "ccf", "survey_acronym", grep("^cpi", names(x), value = TRUE))

  # modifications to the database
  x[,
    c("cur_adj", "ccf")
    := {

      cur_adj      <-  ifelse(is.na(cur_adj), 1, cur_adj)
      ccf          <-  1/cur_adj

      list(cur_adj, ccf)
    }
  ][
    ,
    `:=`(
      country_code   = code,
      surveyid_year  = year,
      reference_year = ref_year,
      cpi            = get(cpivar),
      survey_acronym = survname
    )
  ][,
    cpi_domain := as.character(cpi_domain)

    ][,
      # This part should not exist if the raw data
      # has been properly created
      cpi_data_level := fcase(
        cpi_domain %chin% c("urban/rural", "2") & cpi_data_level == "0", "rural",
        cpi_domain %chin% c("urban/rural", "2") & cpi_data_level == "1", "urban",
        cpi_domain %chin% c("national", "1")  & cpi_data_level %chin% c("2", "", NA_character_) , "national",
        default =  ""
      )
    ]
  # keep final vars
  x <- x[
    ,
    ..keep_vars
  ]

  x <- unique(x)  # remove duplicates
  return(x)
}


