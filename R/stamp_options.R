#' Set pipaux versioning mode
#'
#' Controls how auxiliary data are versioned when saved.
#'
#' @param mode One of:
#'   - "content"   (default) Save only when content or code changes
#'   - "timestamp" Save on every call
#'   - "off"       Disable versioning (overwrite current file)
#'
#' @export
pipaux_set_versioning <- function(mode = c("content", "timestamp", "off")) {
  mode <- match.arg(mode)
  stamp::st_opts(versioning = mode)
  invisible(mode)
}

#' Get current pipaux versioning mode
#'
#' @export
pipaux_get_versioning <- function() {
  stamp::st_opts_get()$versioning
}

#' Reset pipaux stamp options to defaults
#'
#' @export
pipaux_reset_stamp_options <- function() {
  stamp::st_opts_set(
    versioning = "content",
    retain_versions = Inf,
    force_on_code_change = TRUE,
    code_hash = TRUE
  )
  invisible(TRUE)
}

#' Set stamp options (more advanced)
#'
#' Advanced helper that forwards arguments to `stamp::st_opts_set()`.
#' Most users should prefer `pipaux_set_versioning()`.
#'
#' @param ... Options passed to `stamp::st_opts_set()`
#'
#' @export
pipaux_set_stamp_option <- function(...) {
  stamp::st_opts_set(...)
  invisible(TRUE)
}
