# Fetch WB country metadata
u <- 'https://api.worldbank.org/v2/country?format=json&per_page=350'
wdi_metadata <- jsonlite::fromJSON(u, flatten = TRUE)[[2]]

# Create vector with different types of aggregate codes
wdi_aggregate_codes <-
  unique(with(wdi_metadata,
              c(region.id, adminregion.id,
                incomeLevel.id, lendingType.id,
                country_metadata[region.value == 'Aggregates',]$id)))
wdi_aggregate_codes <- wdi_aggregate_codes[!wdi_aggregate_codes == 'NA']

# Save internal package vectors
usethis::use_data(wdi_aggregate_codes,
                  internal = TRUE,
                  overwrite = TRUE,
                  compress = "bzip2",
                  version = 3
)
