#' Verify input
#'
#' @param df data.frame: A table with raw data.
#' @param x character: Type of dataset.
#' @export
#' @return data.frame
verify_input <- function(df, x){
  select_verify_input[[x]](df)
}

#' Select verify input
#' @inheritParams verify_input
#' @noRd
#' @return data.frame
select_verify_input <- list(
  maddison = function(...) verify_input_maddison(...),
  censoring = function(...) verify_input_censoring(...)
)

#' Verify output
#'
#' @inheritParams verify_input
#' @export
#' @return data.frame
verify_output <- function(df, x){
  select_output_input[[x]](df)
}

#' Select verify input
#' @inheritParams verify_input
#' @noRd
#' @return data.frame
select_output_input <- list(
  maddison = function(...) verify_output_maddison(...),
  censoring = function(...) verify_output_censoring(...)
)
