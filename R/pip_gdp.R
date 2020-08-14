#' Update and load GDP data in PIP Auxiliary data structure
#'
#' @param action
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_gdp <- function(action          = "update",
                    maddison_action = "load",
                    force           = FALSE) {


  # update Maddision Project Data
  if (maddison_action == "update") {
    pip_maddison(force = force)
  }


  if (action == "update") {

    madd <- pip_maddison("load")
    wgdp <- wbstats::wb_data(indicator = "NY.GDP.PCAP.KD")
    setDT(madd)
    setDT(wgdp)

    #--------- clean GDP from WDI ---------

    # rename vars
    setnames(wgdp,
             old = c("iso3c", "date", "NY.GDP.PCAP.KD"),
             new = c("country_code", "year", "wdi_gdp")
             )

    # keep relevant variables
    gdp <- wgdp[,
              .(country_code, year, wdi_gdp)
              ]

    # Join Maddison and WDI
    gdp[
        madd,
        on      = .(country_code, year),
        mdp_gdp := i.mdp_gdp
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





  }  # End of update








} # end of pip_gdp

