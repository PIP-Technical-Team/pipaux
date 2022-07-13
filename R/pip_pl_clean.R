#' Build a data table for each list from yaml file with poverty lines info
#'
#' @param l list from yaml file
#'
#' @return data.table
#' @export
pip_pl_clean <- function(l) {

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  pls <-
    purrr::map(.x = l$ranges,
               .f = ~{
                 seq(.x$min, .x$max, .x$increment)
               }) |>
    unlist()

  # Create data frame
  df <- data.table::data.table(
    name = as.character(pls),
    poverty_line = pls
  )


  df[,
     c("is_default", "is_visible", "name", "ppp_year")
     := {
       id   <- fifelse(name == l$default, TRUE, FALSE)

       iv   <- fifelse(name %in% l$visible, TRUE, FALSE)

       n <- fifelse(n_decimals(poverty_line) == 1, paste0(name, "0"), name)
       n <- fifelse(n_decimals(poverty_line) == 0, paste0(n, ".00"), n)

       list(id, iv, n, l$ppp_year)
     }]

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(df)

}
