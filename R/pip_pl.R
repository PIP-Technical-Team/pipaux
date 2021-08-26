#' Poverty lines
#'
#' Update or load a dataset with poverty lines.
#'
#' @inheritParams pip_prices
#' @param pl_default character: Default poverty line
#' @param pl_visible character: Visible poverty lines
#' @export
pip_pl <- function(action = "update",
                   force = FALSE,
                   pl_default = "1.9",
                   pl_visible = c("1.9", "3.2", "5.5"),
                   maindir = getOption("pipaux.maindir")) {
  measure <- "pl"
  msrdir <- paste0(maindir, "_aux/", measure, "/")

  if (action == "update") {
    pls <- seq(0.05, 6.50, 0.05)
    df <- data.table::data.table(
      name = as.character(pls),
      poverty_line = pls
    )
    df$is_default <- ifelse(df$name == pl_default, TRUE, FALSE)
    df$is_visible <- ifelse(df$name %in% pl_visible, TRUE, FALSE)

    # df <- suppressMessages(
    #   readr::read_csv(paste0(maindir, "_aux/pl/poverty_lines.csv"))
    # )
    # df$name <- as.character(df$name)

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
