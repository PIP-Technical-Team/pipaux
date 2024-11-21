#' Fake PIP SNA function
#'
#' @inheritParams aux_gdp
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
fake_aux_sna <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch),
                    from    = c("gh", "file", "api")) {

   return(invisible(TRUE))
}
