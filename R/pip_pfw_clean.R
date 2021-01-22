#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`.
#' @param pfw_id character: CPI and PPP ID. Extracted from `pip_cpi_update()`
#' "icp2011".
#'
#' @export
pip_pfw_clean <- function(y, pfw_id) {
  x <- data.table::as.data.table(y)

  # get just inpovcal data


  # change variable names
  old_var <-
    c("region",
      "code",
      "ref_year",
      "survname",
      'comparability',
      'datatype',
      'rep_year')

  new_var <-
    c(
      "region_code",
      "country_code",
      "survey_year",
      "survey_acronym",
      'survey_comparability',
      'welfare_type',
      'reporting_year'
    )

  setnames(x,
           old = old_var,
           new = new_var)

  # Recode some variables

  x[,
    `:=`(
      # Recode survey coverage
      survey_coverage = fcase(
        survey_coverage == "N", "national",
        survey_coverage == "R", "rural",
        survey_coverage == "U", "urban",
        default = ""
      ),
      # Recode welfare type
      welfare_type = fcase(
        grepl("[Ii]", welfare_type), "income",
        grepl("[Cc]", welfare_type), "consumption",
        default = ""
      ),
      surveyid_year  = as.integer(surveyid_year)
    )
  ]


  # Create Price Framework ID
  x[,
    pfw_id := (pfw_id)
    ]
  x <- unique(x)  # remove duplicates
  data.table::setDT(x)
  return(x)
}


