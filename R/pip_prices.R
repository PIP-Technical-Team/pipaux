#' PIP Prices
#'
#' Works with either CPI, PPP or PFW.
#'
#' @param measure character: Measure to be used. e.g., "cpi" or "ppp".
#' @param action character: Either "load" or "update". Default is "update". If
#' "update" data will be updated on the system. If "load" data is loaded in memory.
#' @param maindir character: Main directory of project.
#' @param dlwdir character: Datalibweb directory.
#' @param force logical: If TRUE data will be overwritten.
#'
#' @export
#' @import data.table
pip_prices <- function(measure  = NULL,
                       action   = "update",
                       maindir  = getOption("pipaux.maindir"),
                       dlwdir   = getOption("pipaux.dlwdir"),
                       force    = FALSE
){


  #----------------------------------------------------------
  #   conditions
  #----------------------------------------------------------

  action <- tolower(action) # convert to lower case just in case

  # Proper length
  if (length(action) != 1) {
    rlang::abort(c(
      "`action` should be length 1",
      x = paste0("`action` is length ", length(action))
    ),
    class = "pipaux_error")

  }

  # proper options
  action_options <- c("load", "update")

  if (!(action  %in% action_options)) {
    action_options <- paste("`", action_options, "`", sep = "")
    msg <- paste("`action` should be", last_item(action_options, "or"))

    rlang::abort(c(
      msg,
      x = paste0("`action` is ", action)
    ),
    class = "pipaux_error"
    )

  }


  #----------------------------------------------------------
  #   define parameters
  #----------------------------------------------------------

  # Always call common values
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir

  #----------------------------------------------------------
  #   execute selected function
  #----------------------------------------------------------

  #--------- load ---------
  if (action == "load") {
    df <- load_aux(msrdir  = msrdir,
                   measure = measure)
    return(df)
  }

  #--------- update ---------
  if (action == "update") {

    if (measure == "cpi") {

      pip_cpi_update(maindir = maindir,
                     dlwdir = dlwdir,
                     force  = force)

    } else if (measure == "ppp") {

      pip_ppp_update(maindir = maindir,
                     dlwdir = dlwdir,
                     force  = force)

    } else if (measure == "pfw") {

      pip_pfw_update(maindir = maindir,
                     dlwdir = dlwdir,
                     force  = force)
    } else {
      rlang::abort(c(
        "The measure selected is not a valid name",
        i = "you can use `cpi`, `ppp`, or `pfw`", # update this message automatically
        x = paste("you selected", measure)
      ),
      class = "pipaux_error"
      )
    }
  } # end of update

} # end of pip_prices function

