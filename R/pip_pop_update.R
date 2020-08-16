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

    pop <- pop[, c("country_code", "year", "pop_data_level", "pop")]

    pip_sign_save(x       = pop,
                  measure = "pop",
                  msrdir  = msrdir,
                  force   = force)
}

