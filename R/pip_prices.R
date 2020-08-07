#' PIP Prices data. Works with either CPI or PPP data
#'
#' @param measure
#' @param action
#' @param maindir
#' @param dlwdir
#' @param force
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_prices <- function(measure  = NULL,
                       action   = "update",
                       maindir = NULL,
                       dlwdir  = NULL,
                       force   = FALSE
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
  com_values <- pip_aux_values()

  if (is.null(maindir)) {
    maindir <- com_values$maindir
  }

  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir

  #----------------------------------------------------------
  #   execute selected function
  #----------------------------------------------------------

  #--------- load ---------
  if (action == "load") {
    df <- pip_aux_load(msrdir  = msrdir,
                       measure = measure)
    return(df)
  }

  #--------- update ---------
  if (action == "update"){
    if (is.null(dlwdir)) {
      dlwdir <- com_values$dlwdir
    }

    if (measure == "cpi") {

    pip_cpi_update(msrdir = msrdir,
                   dlwdir = dlwdir,
                   force  = force)
    }

    if (measure == "ppp") {

    pip_ppp_update(msrdir = msrdir,
                   dlwdir = dlwdir,
                   force  = force)
    }

  }

}

