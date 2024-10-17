#' Send an email that contains auxiliary data validation report
#'
#' @import blastula
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
