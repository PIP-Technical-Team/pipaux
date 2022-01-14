#' Verify input
#'
#' Verify that a raw data object (directly from source) has the correct structure.
#'
#' @param x Data object to validate.
#' @param measure character: Measure that the data object belongs to.
#' @export
verify_input <- function(x, measure){
  select_verify_input[[measure]](x)
}

#' Select verify input
#' @inheritParams verify_input
#' @noRd
select_verify_input <- list(
  maddison = function(...) verify_input_maddison(...),
  censoring = function(...) verify_input_censoring(...),
  cpi = function(...) verify_input_cpi(...),
  country_profiles = function(...) verify_input_country_profiles(...)

)

#' Verify output
#'
#' Verify that a transformed data object has the correct structure.
#'
#' @inheritParams verify_input
#' @export
verify_output <- function(x, measure){
  select_output_input[[measure]](x)
}

#' Select verify input
#' @inheritParams verify_input
#' @noRd
select_verify_input <- list(
  maddison = function(...) verify_output_maddison(...),
  censoring = function(...) verify_output_censoring(...),
  cpi = function(...) verify_ouptut_cpi(...),
  country_profiles = function(...) verify_output_country_profiles(...)
)
