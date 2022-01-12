load_pfw <- function(dir, pattern = "Survey_price_framework\\.dta$"){

  # Check for last version in dlw
  files <- fs::dir_ls(dir, pattern, recurse = TRUE, type = "file")
  latest_pfw <- max(files)
  pfw_id <- gsub("(.*/Support_2005_)([^/]+)(/Data.*)", "\\2", latest_pfw) #Note this is uses in the cleaning
  df <- haven::read_dta(latest_pfw)

  return(df)
}

tranform_pfw <- function(df, pfw_id){

  dt <- data.table::as.data.table(dt)

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


  # Create Price Framework ID
  dt[
    ,
    pfw_id := (pfw_id)
  ]
  dt <- unique(dt)

  # Remove any non-WDI countries
  # pfw <- pfw[country_code %in% cl$country_code]


  return(dt)
}
