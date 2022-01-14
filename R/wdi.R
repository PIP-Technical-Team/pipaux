fetch_wdi <- function(indicator){

  # Create url
  base_url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/%s",
    "?format=json&per_page=%s&page=1"
  )
  the_url <- sprintf(base_url, indicator, 32500)

  # Fetch data
  df <- jsonlite::fromJSON(the_url, flatten = TRUE)[[2]]

  return(df)

}

transform_wdi <- function(df, value_var){

  # Remove aggregates
  df <- df[!df$countryiso3code %in% wdi_aggregate_codes, ]

  # Select columns
  df <- df[c('countryiso3code', 'date', 'value')]

  # Rename columns
  names(df) <- c('country_code', 'year', value_var)

  return(df)
}


