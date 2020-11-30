#' Update population data
#'
#' @param src character: Source fo Population data. Default is `wdi`. Other
#' option is `emi`
#' @param msrdir character: Directory path of measure. Comes from `pip_pop()`
#' @param force logical: If TRUE Force update of population data
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_pop_update <- function(force, src, msrdir) {

  codes <- c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")
  if (src == "wdi") {
    pop <- purrr::map_df(codes, ~{
                        df <- wbstats::wb_data(indicator = .x, lang = "en")
                        colnames(df)[colnames(df) == .x] <- "pop"
                        df$coverage <- .x
                        return(df)
                      })

    setDT(pop)

    # data level
    pop[,
        pop_data_level :=
          fcase(
            grepl("POP", coverage), 2,
            grepl("RUR", coverage), 0,
            grepl("URB", coverage), 1
          )
        ]
    setnames(pop,
             old = c("iso3c", "date"),
             new = c("country_code", "year"))

  } else if (src == "emi") {

  } else {
    msg <- paste("src `", src,"` is not a valid source.")
    rlang::abort(c(
      msg,
      i = "make sure you select `wdi` or `emi`"
    ),
    class = "pipaux_error"
    )
  }

    pop <- pop[, c("country_code", "year", "pop_data_level", "pop")
               ][,
                 pop_domain := fifelse(pop_data_level == 2, 1, 2)
                 ]

    # recode domain and data_level variables
    cols <- c("pop_domain", "pop_data_level")
    pop[,
        (cols) := lapply(.SD, as.character),
        .SDcols = cols
    ][,# recode domain
      pop_domain := fcase(
        pop_domain == "1", "national",
        pop_domain == "2", "urban/rural",
        pop_domain == "3", "subnational region"
      )
    ][   # Recode data_level only for those that are national or urban/rural
      pop_domain %in% c("national", "urban/rural"),
      pop_data_level := fcase(
        pop_data_level == "0", "rural",
        pop_data_level == "1", "urban",
        pop_data_level == "2", "national"
      )
    ]

    pip_sign_save(x       = pop,
                  measure = "pop",
                  msrdir  = msrdir,
                  force   = force)
}

