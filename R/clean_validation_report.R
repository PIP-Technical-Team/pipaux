#' Remove data validation report from .pipaux environment variable
#'
#' @export
clean_validation_report <- function(){

  if (rlang::env_has(.pipaux, "validation_report")){

    # rlang::env_bind(.pipaux, validation_report = rlang::zap())
    rlang::env_unbind(.pipaux, "validation_report")

  }
}
