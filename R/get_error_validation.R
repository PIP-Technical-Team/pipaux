#' Get validation report  data validation error report
#'
#' @param vlddata validation data
#' @import rlang
#'
#' @export
get_error_validation <- function(vlddata, detail){

  stopifnot("Validation data is not availabel" = !is.null(vlddata))

  err_t <- NULL

  if (any(vlddata$type == "error")){

    err_t <-  vlddata[type == "error",
                      .(table_name, description, call,
                        message, type)]
  }


  if (isFALSE(detail)) {

    cli::cli_abort("Description of invalid cases for {unique(err_t$table_name)},
                   {err_t$description}")

  } else {

    if (!rlang::env_has(.pipaux, "validation_report")){

      rlang::env_poke(.pipaux, "validation_report", err_t)

    } else {

      compiled_result <- rbind(.pipaux$validation_report, err_t)
      rlang::env_poke(.pipaux, "validation_report", compiled_result)

    }
  }

}
