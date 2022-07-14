#' Clean PFW
#'
#' Clean PFW data from Datalibweb to meet PIP protocols.
#'
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`.
#' @inheritParams load_aux
#'
#' @keywords internal
pip_pfw_clean <- function(y,
                          maindir = gls$PIP_DATA_DIR,
                          branch  = c("DEV", "PROD", "main")) {

  branch <- match.arg(branch)

  if (!inherits(y, "data.table")) {
    x <- as.data.table(y)
  } else {
    x <- copy(y)
  }

  # get just inpovcal data


  # change variable names
  old_var <-
    c(
      "region",
      "reg_pcn",
      "code",
      "ref_year",
      "survname",
      "comparability",
      "datatype",
      "rep_year"
    )

  new_var <-
    c(
      "wb_region_code",
      "pcn_region_code",
      "country_code",
      "survey_year",
      "survey_acronym",
      "survey_comparability",
      "welfare_type",
      "reporting_year"
    )

  setnames(x,
    old = old_var,
    new = new_var
  )

  # Recode some variables

  x[
    ,
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
      surveyid_year = as.integer(surveyid_year)
    )
  ]

  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)
  x <- x[country_code %in% cl$country_code]

  x <- unique(x) # remove duplicates
  return(x)
}
