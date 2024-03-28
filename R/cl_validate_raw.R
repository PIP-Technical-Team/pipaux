#' Validate raw country list data
#'
#' @param cl raw country list data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
cl_validate_raw <- function(cl, detail = getOption("pipaux.detail.raw")){

  stopifnot("Country list raw data is not loaded" = !is.null(cl))

  report <- data_validation_report()

  validate(cl, name = "CL raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_cols(in_set(c("ABW", "AFG", "AGO", "ALB", "AND", "ARE", "ARG", "ARM", "ASM", "ATG", "AUS", "AUT", "AZE",
                           "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHR", "BHS", "BIH", "BLR", "BLZ", "BMU", "BOL",
                           "BRA", "BRB", "BRN", "BTN", "BWA", "CAF", "CAN", "CHE", "CHI", "CHL", "CHN", "CIV", "CMR",
                           "COD", "COG", "COL", "COM", "CPV", "CRI", "CUB", "CUW", "CYM", "CYP", "CZE", "DEU", "DJI",
                           "DMA", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH", "FIN", "FJI", "FRA",
                           "FRO", "FSM", "GAB", "GBR", "GEO", "GHA", "GIB", "GIN", "GMB", "GNB", "GNQ", "GRC", "GRD",
                           "GRL", "GTM", "GUM", "GUY", "HKG", "HND", "HRV", "HTI", "HUN", "IDN", "IMN", "IND", "IRL",
                           "IRN", "IRQ", "ISL", "ISR", "ITA", "JAM", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR",
                           "KNA", "KOR", "KWT", "LAO", "LBN", "LBR", "LBY", "LCA", "LIE", "LKA", "LSO", "LTU", "LUX",
                           "LVA", "MAC", "MAF", "MAR", "MCO", "MDA", "MDG", "MDV", "MEX", "MHL", "MKD", "MLI", "MLT",
                           "MMR", "MNE", "MNG", "MNP", "MOZ", "MRT", "MUS", "MWI", "MYS", "NAM", "NCL", "NER", "NGA",
                           "NIC", "NLD", "NOR", "NPL", "NRU", "NZL", "OMN", "PAK", "PAN", "PER", "PHL", "PLW", "PNG",
                           "POL", "PRI", "PRK", "PRT", "PRY", "PSE", "PYF", "QAT", "ROU", "RUS", "RWA", "SAU", "SDN",
                           "SEN", "SGP", "SLB", "SLE", "SLV", "SMR", "SOM", "SRB", "SSD", "STP", "SUR", "SVK", "SVN",
                           "SWE", "SWZ", "SXM", "SYC", "SYR", "TCA", "TCD", "TGO", "THA", "TJK", "TKM", "TLS", "TON",
                           "TTO", "TUN", "TUR", "TUV", "TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VCT", "VEN",
                           "VGB", "VIR", "VNM", "VUT", "WSM", "XKX", "YEM", "ZAF", "ZMB", "ZWE")),
                  country_code, description = "`country_code` values within range") |>
    validate_if(is.character(country_name),
                description = "`country_name` should be character") |>
    validate_if(is.character(africa_split),
                description = "`africa_split` should be character") |>
    validate_cols(in_set(c("Eastern and Southern Africa", "Western and Central Africa", NA)),
                  africa_split, description = "`africa_split` values within range") |>
    validate_if(is.character(africa_split_code),
                description = "`africa_split_code` should be character") |>
    validate_cols(in_set(c("AFE", "AFW", NA)),
                  africa_split_code, description = "`africa_split_code` values within range") |>
    validate_if(is.character(pcn_region),
                description = "`pcn_region` should be character") |>
    validate_if(is.character(pcn_region_code),
                description = "`pcn_region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  pcn_region_code, description = "`pcn_region_code` values within range") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    validate_if(is.character(region_code),
                description = "`region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  region_code, description = "`region_code` values within range") |>
    validate_if(is.character(world),
                description = "`world` should be character") |>
    validate_cols(in_set(c("World")),
                  world, description = "`world` values within range") |>
    validate_if(is.character(world_code),
                description = "`world_code` should be character") |>
    validate_cols(in_set(c("WLD")),
                  world_code, description = "`world_code` values within range") |>
    validate_cols(not_na, country_code,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

