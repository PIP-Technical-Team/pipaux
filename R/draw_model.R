#' Draw data model of aux data
#'
#' @param pattern character: pattern to be used in [tidyselect::matches] inside
#'   [dm::dm_select_tbl].
#' @param ... parameters parsed to [dm::dm_draw]
#' @param perl logical: whether to use perl matching. Default FALSE.
#'
#' @return `grViz` object from {dm} package
#' @export
#' @examples
#' # draw country profile
#' draw_model("^cp_")
#'
#' # all but country profile data
#' draw_model(pattern = "^(?!cp_).+", perl = TRUE)
#'
#' # just titles
#' draw_model(pattern = "^(?!cp_).+",
#' perl = TRUE,
#' view_type = "title_only")
#'
draw_model <- function(pattern = "*",
                       perl    = FALSE,
                       ...) {

  aux_data_model |>
    dm::dm_select_tbl(matches(pattern, perl = perl)) |>
    dm::dm_draw(...)

}




