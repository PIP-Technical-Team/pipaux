#' Poverty lines
#'
#' Update or load a dataset with poverty lines.
#'
#' @inheritParams pip_prices
#' @export
pip_pl <- function(action = "update",
                   force = FALSE,
                   maindir = gls$PIP_DATA_DIR) {
  measure <- "pl"
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (action == "update") {
    # Read yaml file
    dl <- yaml::read_yaml(fs::path(msrdir, "poverty-lines.yaml"))

    # Create vector with poverty lines
    tmp <- vector("list", length = length(dl$min))
    for (i in seq_along(dl$min)) {

      tmp[[i]] <- seq(dl$min[i], dl$max[i], dl$increment[i])
    }
    pls <- unlist(tmp)

    # Create data frame
    df <- data.table::data.table(
      name = as.character(pls),
      poverty_line = pls
    )
    df$is_default <- ifelse(df$name == dl$default, TRUE, FALSE)
    df$is_visible <- ifelse(df$name %in% dl$visible, TRUE, FALSE)
    df$name <- ifelse(n_decimals(df$poverty_line) == 1, sprintf("%s0", df$name), df$name)
    df$name <- ifelse(n_decimals(df$poverty_line) == 0, sprintf("%s.00", df$name), df$name)

    pip_sign_save(
      x = df,
      measure = measure,
      msrdir = msrdir,
      force = force
    )
  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  } else {
    msg <- paste("action `", action, "` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }
}
