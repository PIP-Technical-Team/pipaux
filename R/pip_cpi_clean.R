#' Clean CPI data
#'
#' Clean CPI data from Datalibweb to meet PIP protocols.
#'
#' @param y dataset with CPI data from `pip_cpi_update()`.
#' @param cpivar character: CPI variable to be used as default. Currently it is
#' "cpi2011".
#' @inheritParams pip_cpi_update
#'
#' @keywords internal
pip_cpi_clean <- function(y,
                          cpivar = getOption("pipaux.cpivar"),
                          maindir = gls$PIP_DATA_DIR,
                          branch  = c("DEV", "PROD", "main", "test")) {

  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c(
    "country_code", "cpi_year", "survey_year",
    "cpi", "ccf", "survey_acronym", "change_cpi2011",
    grep("^cpi", names(x), value = TRUE)
  )

  # modifications to the database
  x[
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
      cpi            = get(cpivar),
      survey_acronym = survname,
      cpi_domain     = as.character(cpi_domain),
      cpi_data_level = as.character(cpi_data_level)
    )
  ][
    ,
    # This part should not exist if the raw data
    # had been created properly
    cpi_data_level := fcase(
      tolower(cpi_domain) %chin% c("urban/rural", "2") & cpi_data_level == "0", "rural",
      tolower(cpi_domain) %chin% c("urban/rural", "2") & cpi_data_level == "1", "urban",
      tolower(cpi_domain) %chin% c("national", "1") & cpi_data_level %chin% c("2", "", NA_character_), "national",
      default = ""
    )
  ]
  # keep final vars
  x <- x[, ..keep_vars ]

  x <- unique(x) # remove duplicates

  # Remove any non-WDI countries
  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)

  x <- x[country_code %in% cl$country_code]


  return(x)
}
