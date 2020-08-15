#' Update GDP data using WDI, Maddison and Special cases.
#'
#' @param force logical: If TRUE force update of GDP data
#'
#' @return
#' @export
#'
#' @examples
pip_gdp_update <- function(force){

  r         <- pip_aux_values()
  #----------------------------------------------------------
  #   Load data
  #----------------------------------------------------------

  madd <- pip_maddison("load")
  wgdp <- wbstats::wb_data(indicator = "NY.GDP.PCAP.KD", lang = "en")
  setDT(madd)
  setDT(wgdp)

  #--------- clean GDP from WDI ---------

  # rename vars
  setnames(wgdp,
           old = c("iso3c", "date", "NY.GDP.PCAP.KD"),
           new = c("country_code", "year", "wdi_gdp")
  )


  #----------------------------------------------------------
  #   Clean GDP data
  #----------------------------------------------------------

  # keep relevant variables
  gdp <- wgdp[,
              .(country_code, year, wdi_gdp)
  ]

  # Join Maddison and WDI
  gdp[
    madd,
    on      = .(country_code, year),
    mpd_gdp := i.mpd_gdp
  ]

  # data now used in Maddison
  gdp[
    year >= 2000,
    mpd_gdp := NA
  ]

  # Special cases for IND, IDN, and CHN
  sp <- gdp[country_code %chin% c("IND", "IDN", "CHN")]

  # Expand three time these cases using cross-join.
  sp <- sp[CJ(gdp_data_level = c(0, 1),
              country_code = country_code,
              year = year,
              unique = TRUE),
           on = .(country_code, year)
  ]

  gdp[,gdp_data_level := 2]

  # append
  gdp <- rbindlist(list(gdp, sp))

  setorder(gdp, country_code, year, gdp_data_level)

  #--------- Load National Accounts Special cases ---------
  #Note: we could add a function to convert csv in fst format... for other time
  measure   <- "sna"
  msrdir    <- paste0(r$maindir, "_aux/", measure, "/")  # measure dir
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
    gdp_data_level := fcase(
      coverage == "national", 2,
      coverage == "urban",    1,
      coverage == "rural",    0
    )
  ]

  #--------- Join SNA with GDP data ---------
  gdp$master <- 1
  sna$using  <- 2

  gdp[sna,
      on = .(country_code, year, gdp_data_level),
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

  #--------- replicate Espen's code ---------
  setorder(gdp, country_code, gdp_data_level, year)

  # init new GDP variable
  gdp[,
      new_gdp := fifelse(merge ==1, wdi_gdp, sna_gdp)
  ][,
    # Lagged and lead values of new GDP
    `:=`(
      new_gdp_lag  = shift(new_gdp),
      new_gdp_lead = shift(new_gdp, type = "lead")
    ),
    by = .(country_code, gdp_data_level)
  ][
    , # Row ID by country and coverage
    n := rowid(country_code, gdp_data_level)
  ]

  # Linking factors
  gdp[,
      `:=`(
        # linking factors back
        bck = (!is.na(new_gdp)
               & !is.na(new_gdp_lag)
               & n != 1) * (new_gdp/mpd_gdp),

        # linking factors forward
        fwd = (!is.na(new_gdp)
               & !is.na(new_gdp_lead)
               & n != .N) * (new_gdp/mpd_gdp)
      ),
      by = .(country_code, gdp_data_level)
  ][,
    `:=`(
      # Max value in linking factor
      bcki = max(bck, na.rm = TRUE),
      fwdi = max(fwd, na.rm = TRUE)
    ),
    by = .(country_code, gdp_data_level)
  ]

  # Apply: create linked value
  gdp[,
      `:=`(
        vbck = mpd_gdp * bcki,
        vfwd = mpd_gdp * fwdi
      )
  ]

  # Assess where to apply forward of backward
  gdp[,
      gapsum := {
        gap     <- (is.na(new_gdp) & !is.na(new_gdp_lag))
        gap     <- fifelse(n == 1, TRUE, gap)
        gapsum  <-  sum(gap)
      },
      by = .(country_code, gdp_data_level)
  ]

  # Replace where missing and indicate source
  gdp[
    ,
    gdp := {
      # fwd
      new_gdp = fifelse(is.na(new_gdp) & gapsum == 2, vfwd, new_gdp)
      # bck
      new_gdp = fifelse(is.na(new_gdp) & gapsum == 1, vbck, new_gdp)
    }
  ]

  gdp <- gdp[!is.na(gdp) & !is.infinite(gdp),
             c("country_code", "year", "gdp_data_level", "gdp")
  ][,
    gdp_domain := fifelse(gdp_data_level == 2, 1, 2)
  ]


  #----------------------------------------------------------
  #   Save and data signature
  #----------------------------------------------------------
  measure   <- "gdp"
  msrdir    <- paste0(r$maindir, "_aux/", measure, "/")  # measure dir

  pip_sign_save(x       = gdp,
                measure = "gdp",
                msrdir  = msrdir,
                force   = force)

}
