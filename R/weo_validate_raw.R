#' Validate raw weo data
#'
#' @param weo raw weo data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
weo_validate_raw <- function(weo, detail = getOption("pipaux.detail.raw")){

  stopifnot("WEO raw data is not loaded" = !is.null(weo))

  report <- data_validation_report()

  weo <- weo[!is.na(`WEO Subject Code`), ]

  validate(weo, name = "WEO raw data validation") |>
    validate_if(is.character(`WEO Country Code`),
                description = "`WEO Country Code` should be character") |>
    validate_if(is.character(ISO),
                description = "ISO should be character") |>
    validate_if(is.character(`WEO Subject Code`),
                description = "`WEO Subject Code` should be character") |>
    validate_if(is.character(Country),
                description = "`Country` should be character") |>
    validate_if(is.character(`Subject Descriptor`),
                description = "`Subject Descriptor` should be character") |>
    validate_if(is.character(`Subject Notes`),
                description = "`Subject Notes` should be character") |>
    validate_if(is.character(Units),
                description = "`Units` should be character") |>
    validate_if(is.character(Scale),
                description = "`Scale` should be character") |>
    validate_if(is.character(`Country/Series-specific Notes`),
                description = "`Country/Series-specific Notes` should be character") |>
    validate_if(is.character(`1980`),
                description = "`1980` should be character") |>
    validate_if(is.character(`1981`),
                description = "`1981` should be character") |>
    validate_if(is.character(`1982`),
                description = "`1982` should be character") |>
    validate_if(is.character(`1983`),
                description = "`1983` should be character") |>
    validate_if(is.character(`1984`),
                description = "`1984` should be character") |>
    validate_if(is.character(`1985`),
                description = "`1985` should be character") |>
    validate_if(is.character(`1986`),
                description = "`1986` should be character") |>
    validate_if(is.character(`1987`),
                description = "`1987` should be character") |>
    validate_if(is.character(`1988`),
                description = "`1988` should be character") |>
    validate_if(is.character(`1989`),
                description = "`1989` should be character") |>
    validate_if(is.character(`1990`),
                description = "`1990` should be character") |>
    validate_if(is.character(`1991`),
                description = "`1991` should be character") |>
    validate_if(is.character(`1992`),
                description = "`1992` should be character") |>
    validate_if(is.character(`1993`),
                description = "`1993` should be character") |>
    validate_if(is.character(`1994`),
                description = "`1994` should be character") |>
    validate_if(is.character(`1995`),
                description = "`1995` should be character") |>
    validate_if(is.character(`1996`),
                description = "`1996` should be character") |>
    validate_if(is.character(`1997`),
                description = "`1997` should be character") |>
    validate_if(is.character(`1998`),
                description = "`1998` should be character") |>
    validate_if(is.character(`1999`),
                description = "`1999` should be character") |>
    validate_if(is.character(`2000`),
                description = "`2000` should be character") |>
    validate_if(is.character(`2001`),
                description = "`2001` should be character") |>
    validate_if(is.character(`2002`),
                description = "`2002` should be character") |>
    validate_if(is.character(`2003`),
                description = "`2003` should be character") |>
    validate_if(is.character(`2004`),
                description = "`2004` should be character") |>
    validate_if(is.character(`2005`),
                description = "`2005` should be character") |>
    validate_if(is.character(`2006`),
                description = "`2006` should be character") |>
    validate_if(is.character(`2007`),
                description = "`2007` should be character") |>
    validate_if(is.character(`2008`),
                description = "`2008` should be character") |>
    validate_if(is.character(`2009`),
                description = "`2009` should be character") |>
    validate_if(is.character(`2010`),
                description = "`2010` should be character") |>
    validate_if(is.character(`2011`),
                description = "`2011` should be character") |>
    validate_if(is.character(`2012`),
                description = "`2012` should be character") |>
    validate_if(is.character(`2013`),
                description = "`2013` should be character") |>
    validate_if(is.character(`2014`),
                description = "`2014` should be character") |>
    validate_if(is.character(`2015`),
                description = "`2015` should be character") |>
    validate_if(is.character(`2016`),
                description = "`2016` should be character") |>
    validate_if(is.character(`2017`),
                description = "`2017` should be character") |>
    validate_if(is.character(`2018`),
                description = "`2018` should be character") |>
    validate_if(is.character(`2019`),
                description = "`2019` should be character") |>
    validate_if(is.character(`2020`),
                description = "`2020` should be character") |>
    validate_if(is.character(`2021`),
                description = "`2021` should be character") |>
    validate_if(is.character(`2022`),
                description = "`2022` should be character") |>
    validate_if(is.character(`2023`),
                description = "`2023` should be character") |>
    validate_if(is.character(`2024`),
                description = "`2024` should be character") |>
    validate_if(is.character(`2025`),
                description = "`2025` should be character") |>
    validate_if(is.character(`2026`),
                description = "`2026` should be character") |>
    validate_if(is.character(`2027`),
                description = "`2027` should be character") |>
    validate_if(is.character(`2028`),
                description = "`2028` should be character") |>
    validate_if(is.numeric(`Estimates Start After`),
                description = "`Estimates Start After` should be numeric") |>
    validate_cols(not_na, ISO, `WEO Subject Code`,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(ISO, `WEO Subject Code`),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

