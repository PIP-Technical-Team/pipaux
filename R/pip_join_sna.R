#' pip_join_sna
#'
#' @param x data frame, either gdp or pce
#' @param measure character: either "gdp" or "pce"
#'
#' @return data.table
#' @export
pip_join_sna <- function(x, measure) {

  # Special cases for IND, IDN, and CHN
  sp <- x[country_code %chin% c("IND", "IDN", "CHN")]

  # Expand three time these cases using cross-join.
  sp <- sp[CJ(data_level   = c(0, 1),
              country_code = country_code,
              year         = year,
              unique       = TRUE),
           on = .(country_code, year)
          ]

  x[, data_level := 2]

  # append
  x <- rbindlist(list(x, sp))

  setorder(x, country_code, year, data_level)

  #--------- Load National Accounts Special cases ---------
  #Note: we could add a function to convert csv in fst format... for other time
  msrdir    <- paste0(getOption("pipaux.maindir"), "_aux/sna/")  # measure dir
  # most recent file
  sna_file  <- max(list.files(msrdir,
                              pattern = "^NAS.+csv$",
                              full.names = TRUE)
  )

  sna <- suppressMessages(readr::read_csv(sna_file))
  sna <- janitor::clean_names(sna)
  setDT(sna)
  setnames(sna,
           old = c("countryname", "countrycode", "gdp", "pce"),
           new = c("country_name", "country_code", "sna_gdp", "sna_pce")
  )
  sna[
    ,
    coverage := tolower(coverage)
  ][
    ,
    data_level := fcase(
      coverage == "national", 2,
      coverage == "urban",    1,
      coverage == "rural",    0
    )
  ]

  #--------- Join SNA with GDP data ---------
  x$master <- 1
  sna$using  <- 2

  x[sna,
      on = .(country_code, year, data_level),
      `:=`(
        sna_gdp = i.sna_gdp,
        sna_pce = i.sna_pce,
        using   = i.using
      )
  ][
    ,
    `:=`(
      master = fifelse(is.na(master), 0, master),
      using = fifelse(is.na(using), 0, using)
    )
  ][,
    merge := master + using # reseambles STata merge
  ]

  dl_name <- paste0(measure, "_data_level")
  setnames(x, "data_level", dl_name)

  return(x)

}
