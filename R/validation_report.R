#' Get validation report  data validation error report
#'
#' @param vlddata validation data
#' @param detail has an option TRUE/FALSE, default value is FALSE
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

    cli::cli_inform("Validation report ({.field validation_report}) has been added to the environment varaible ({.field .pipaux}).")
  }

}

#' Remove data validation report from .pipaux environment variable
#'
#' @export
clean_validation_report <- function(){

  if (rlang::env_has(.pipaux, "validation_report")){

    # rlang::env_bind(.pipaux, validation_report = rlang::zap())
    rlang::env_unbind(.pipaux, "validation_report")

  }
}

#' Send an email that contains auxiliary data validation report
#'
#'
#' @export
send_report <- function(){

  if (rlang::env_has(.pipaux, "validation_report")){

    print(.pipaux$validation_report)

    #   fname <- file.path(tempdir(), "data_validation_report.csv")
    #
    #   write.csv(.pipaux$validation_report, fname, row.names = FALSE)
    #
    #   compose_email(
    #     body = md(glue::glue(
    #
    #       "Hello,
    #
    # The attched file contains auxiliary data validation report.
    #
    # Regards"))) |>
    #     add_attachment(file = fname, filename = "data_validation_report") |>
    #     smtp_send(
    #       from = "tefera.degefu@outlook.com",
    #       to = "tdegefu@worldbank.org",
    #       subject = "Data validation report",
    #       credentials = creds_envvar(user = "tefera.degefu@outlook.com",
    #                                  pass_envvar = "SMTP_GPID_EMAIL",
    #                                  provider = "outlook")
    #     )
  }
}
