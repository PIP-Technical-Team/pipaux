#' Update Auxiliary data. Wrapper of measure-specific functions.
#'
#' @inheritParams aux_labels_pip
#' @inheritParams aux_cpi
#' @inheritParams pipfun::load_from_gh
#' @param verbose logical : Do you want verbose output?
#' @export
update_aux <- function(measure,
                       force   = FALSE,
                       owner   = getOption("pipfun.ghowner"),
                       maindir = gls$PIP_DATA_DIR,
                       branch  = c("DEV", "PROD", "main"),
                       tag     = match.arg(branch),
                       verbose = FALSE
                       ) {

  branch <- match.arg(branch)
  al     <- as.list(environment())  # Arguments List

  pipfun_verbose <- getOption("pipfun.verbose")
  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({
    options(pipfun.verbose = pipfun_verbose)
  })

  options(pipfun.verbose = verbose)



  # Get all the aux table if measure == "all"
  if ("all" %in% tolower(measure)) {
    measure <-
      lsf.str("package:pipaux",
              pattern = "^aux_[a-z]+$") |>
      as.character() |>
      {\(.) gsub("^aux_", "", .)}() |>
      sort()
  }

  # remove measure
  al$measure <- NULL
  al$verbose <- NULL

  # build function name
  fun_name <- glue("aux_{measure}")

  rs <- lapply(fun_name,
               \(.x) {
                 sv <-
                 tryCatch(
                   expr = {

                     x <- do.call(.x, c(action = "update", al))
                     x <- ifelse(isTRUE(x), "saved", "up to date")

                   }, # end of expr section

                   error = function(e) {
                     "failed"
                   }, # end of error section

                   warning = function(w) {
                     paste(x, "with warning")
                   }
                 ) # End of trycatch

               })

  names(rs) <- measure

  return(rs)
}

