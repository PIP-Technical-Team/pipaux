#' Update pce data using WDI, Maddison and Special cases.
#'
#' @inheritParams pip_pce
#'
#' @return
#' @export
#'
#' @examples
pip_pce_update <- function(force){

  #----------------------------------------------------------
  #   Load data
  #----------------------------------------------------------

  wpce <- wbstats::wb_data(indicator = "NE.CON.PRVT.PC.KD", lang = "en")
  setDT(wpce)

  #--------- clean pce from WDI ---------

  # rename vars
  setnames(wpce,
           old = c("iso3c", "date", "NE.CON.PRVT.PC.KD"),
           new = c("country_code", "year", "wdi_pce")
           )

  #----------------------------------------------------------
  #   Clean pce data
  #----------------------------------------------------------

  # keep relevant variables
  pce <- wpce[,
              .(country_code, year, wdi_pce)
             ]

  # Join with Special National Accounts data.
  pce <- pip_join_sna(pce, measure = "pce")

  #--------- replicate Espen's code ---------
  setorder(pce, country_code, pce_data_level, year)

  # init new pce variable
  pce[,
      pce := fifelse(merge ==1, wdi_pce, sna_pce)
    ]

  # Linking factors
  pce <- pce[!is.na(pce) & !is.infinite(pce),
             c("country_code", "year", "pce_data_level", "pce")
  ][,
    pce_domain := fifelse(pce_data_level == 2, 1, 2)
  ]


  # recode domain and data_level variables
  cols <- c("pce_domain", "pce_data_level")
  pce[,
      (cols) := lapply(.SD, as.character),
      .SDcols = cols
      ][,# recode domain
        pce_domain := fcase(
          pce_domain == "1", "national",
          pce_domain == "2", "urban/rural",
          pce_domain == "3", "subnational region"
        )
      ][   # Recode data_level only for those that are national or urban/rural
        pce_domain %in% c("national", "urban/rural"),
        pce_data_level := fcase(
          pce_data_level == "0", "rural",
          pce_data_level == "1", "urban",
          pce_data_level == "2", "national"
        )
      ]

  #----------------------------------------------------------
  #   Save and data signature
  #----------------------------------------------------------
  measure   <- "pce"
  msrdir    <- paste0(getOption("pipaux.maindir"), "_aux/", measure, "/")  # measure dir

  pip_sign_save(x       = pce,
                measure = "pce",
                msrdir  = msrdir,
                force   = force)

}
