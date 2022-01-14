#' Chain values
#'
#' Chain GDP or PCE values.
#'
#' @param dt data.table: A table.
#' @param base_var character: Name of base variable, e.g. 'wdi_gdp'.
#' @param replacement_var character: Name of replacement variable, e.g.
#'   'weo_gdp'.
#' @param new_name character: Name of replacement variable, e.g. 'weo_gdp'.
#' @param by character: A vector with columns to split `dt` by. Defaults to
#'   'country_code'.
#'
#' @keywords internal
chain_values <- function(dt, base_var, replacement_var, new_name, by = "country_code") {

  # anyNA
  anyNA <- function(x) all(is.na(x))

  # Add rowid by group
  dt$n <- data.table::rowidv(dt, cols = by)

  # Check if any groups have missing values for
  # all observations of base_var
  dt_na <- dt[,
              .SDcols = base_var, by = by,
              .(all_na = purrr::map_lgl(.SD, anyNA))
  ]
  dt <- data.table::merge.data.table(dt, dt_na, by = by)

  # Create new_var (equal to base_var or replacement_var
  # if entire series is missing )
  dt$new_var <- data.table::fifelse(
    dt$all_na, dt[[replacement_var]], dt[[base_var]]
  )

  # Create lag and lead columns by group
  dt$rep_var <- dt[[replacement_var]]
  dt[,
     `:=`(
       rep_var_lag = shift(rep_var),
       rep_var_lead = shift(rep_var, type = "lead")
     ),
     by = by
  ]

  # Create linking factors (growth values)
  dt[,
     `:=`(
       # linking factors back
       fwd = (!is.na(rep_var) &
                !is.na(rep_var_lag) &
                n != 1) * (rep_var / rep_var_lag),
       # linking factors forward
       bck = (!is.na(rep_var) &
                !is.na(rep_var_lead) &
                n != .N) * (rep_var / rep_var_lead)
     ),
     by = by
  ]

  # Chain forwards
  dt[, new_var := chain_forwards(.SD), by = by]

  # Chain backwards
  dt[, new_var := chain_backwards(.SD), by = by]

  # Set new name
  data.table::setnames(dt, "new_var", new_name)

  # Remove temporary columns
  dt <- dt[, !c(
    "fwd", "bck", "rep_var", "rep_var_lag",
    "rep_var_lead", "all_na", "n"
  )]

  return(dt)
}

#' chain_forwards
#' @param dt data.table: A country-level data.table.
#' @noRd
chain_forwards <- function(dt) {
  n <- nrow(dt)
  for (i in seq(2, n)) {
    dt$new_var[i] <-
      data.table::fifelse( #
        is.na(dt$new_var[i]) & !is.na(dt$rep_var[i]) & !is.na(dt$fwd[i]),
        dt$new_var[i - 1] * dt$fwd[i],
        dt$new_var[i]
      )
  }
  return(dt$new_var)
}

#' chain_backwards
#' @param dt data.table: A country-level data.table.
#' @noRd
chain_backwards <- function(dt) {
  n <- nrow(dt)
  data.table::setorder(dt, -year)
  for (i in seq(2, n)) {
    dt$new_var[i] <-
      data.table::fifelse(
        is.na(dt$new_var[i]) & !is.na(dt$rep_var[i]) & !is.na(dt$bck[i]),
        dt$new_var[i - 1] * dt$bck[i],
        dt$new_var[i]
      )
  }
  data.table::setorder(dt, year)
  return(dt$new_var)
}
