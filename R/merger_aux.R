#' Merge auxiliary datasets
#'
#' @param aux_data1 auxiliary data one
#' @param aux_data1 auxiliary data two
#' @param merge_type merge/ join type, the default is left join, options (left, right, full, using, master, inner)
#' @param commn_vars option to keep or retain common variables, default is TRUE
#'
#' @return data.table with key information
#' @export
#'
#' @examples
#' pfw <- pipload::load_aux_data("pfw")
#' ppp <- pipload::load_aux_data("ppp")
#' pfw_ppp <- merger_aux(pfw, ppp)
#'
#' cpi <- pipload::load_aux_data("cpi")
#' pfw_cpi <- merger_aux(cpi, pfw, merge_type = "right", commn_vars = FALSE)
#' cpi_pfw <- merger_aux(cpi, pfw)
#'
merger_aux <- function(aux_data1,
                       aux_data2,
                       merge_type = c("left", "right", "full",
                                      "using", "master", "inner"),
                       commn_vars = FALSE,
                       ...) {

  merge_type  <- match.arg(merge_type)
  print(merge_type)

  stopifnot("First data is empty"  = !is.null(aux_data1))
  stopifnot("Second data is empty" = !is.null(aux_data2))

  # extract dataset names
  measure1 <- attr(aux_data1, "aux_name")
  measure2 <- attr(aux_data2, "aux_name")

  print(measure1)
  print(measure2)

  # ---- Special handling for pfw ----
  if (measure1 == "pfw" || measure2 == "pfw") {

    pfw_key <- aux_pfw_key()

    if (measure1 == "pfw") {
      aux_data1 <- pfw_key[
        aux_data1,
        on = .(country_code, survey_year, survey_acronym, cpi_domain_var)
      ]
      setattr(aux_data1, "aux_name", "pfw")
    }

    if (measure2 == "pfw") {
      aux_data2 <- pfw_key[
        aux_data2,
        on = .(country_code, survey_year, survey_acronym, cpi_domain_var)
      ]
      setattr(aux_data2, "aux_name", "pfw")
    }
  }

  # ---- NEW: Retrieve keys using stamp ----
  key_aux_data1 <- stamp::st_get_pk(aux_data1)
  key_aux_data2 <- stamp::st_get_pk(aux_data2)

  if (is.null(key_aux_data1) || is.null(key_aux_data2)) {
    cli::cli_abort("Primary key not found in one of the datasets.")
  }

  int_key <- intersect(key_aux_data1, key_aux_data2)

  if (length(int_key) == 0) {
    cli::cli_abort("No common primary key columns found between datasets.")
  }

  # ---- Determine relationship ----
  isid1 <- joyn::is_id(aux_data1, int_key)
  isid2 <- joyn::is_id(aux_data2, int_key)

  mtype1 <- if (isid1) "1" else "m"
  mtype2 <- if (isid2) "1" else "m"

  mtype <- paste(mtype1, mtype2, sep = ":")

  if (mtype == "m:m") {
    cli::cli_abort("Auxiliary files shouldn't have `m:m` relationship")
  }

  # ---- Perform join ----
  mdata <- joyn::joyn(
    aux_data1,
    aux_data2,
    by = int_key,
    match_type = mtype,
    keep = merge_type,
    keep_common_vars = commn_vars
  )

  # ---- Update metadata ----
  setattr(mdata, "aux_name", paste(measure1, measure2, sep = "_"))

  return(mdata)
}