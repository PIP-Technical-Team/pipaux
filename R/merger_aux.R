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
#' pfw <- load_aux("pfw")
#' ppp <- load_aux("ppp")
#' pfw_ppp <- merger_aux(pfw, ppp)
#'
#' cpi <- load_aux("cpi")
#' pfw_cpi <- merger_aux(cpi, pfw, merge_type = "right", commn_vars = FALSE)
#' cpi_pfw <- merger_aux(cpi, pfw)
#'
merger_aux <- function(aux_data1,
                       aux_data2,
                       merge_type = c("left", "right", "full",
                                "using", "master", "inner"),
                       commn_vars = FALSE,
                       ...
){

  merge_type  <- match.arg(merge_type)
  print(merge_type)

  stopifnot("First data is empty" = !is.null(aux_data1))
  stopifnot("Second data is empty" = !is.null(aux_data2))

  # extract measure name and dataset key's

  if (!is.null(attr(aux_data1, "aux_name"))) measure1 <- attr(aux_data1, "aux_name")
  print(measure1)

  if (!is.null(attr(aux_data2, "aux_name"))) measure2 <- attr(aux_data2, "aux_name")
  print(measure2)

  if (measure1 == "pfw" || measure2 == "pfw"){

    # generate a dataset that can be used to add reporting_level variable to pfw data
    pfw_key <- aux_pfw_key()

    pfw <- pfw_key[pfw, on = .(country_code, survey_year, survey_acronym, cpi_domain_var)]

    setattr(pfw,
            "aux_key",
            c("country_code", "year", "reporting_level", "survey_acronym", "welfare_type"))

    setattr(pfw, "aux_name", "pfw")

    if (measure1 == "pfw") {

      aux_data1 <- pfw_key[aux_data1,
                           on = .(country_code, survey_year, survey_acronym, cpi_domain_var)]

      setattr(aux_data1,
              "aux_key",
              c("country_code", "year", "reporting_level",
                "survey_acronym", "welfare_type"))

      setattr(aux_data1, "aux_name", "pfw")
    }

    if (measure2 == "pfw") {

      aux_data2 <- pfw_key[aux_data2,
                           on = .(country_code, survey_year, survey_acronym, cpi_domain_var)]

      setattr(aux_data2,
              "aux_key",
              c("country_code", "year", "reporting_level",
                "survey_acronym", "welfare_type"))

      setattr(aux_data2, "aux_name", "pfw")
    }

  }

  key_aux_data1 <- attr(aux_data1, "aux_key")
  key_aux_data2 <- attr(aux_data2, "aux_key")

  int_key <- intersect(key_aux_data1, key_aux_data2)

  isid1 <- joyn::is_id(aux_data1, int_key)
  isid2 <- joyn::is_id(aux_data2, int_key)

  mtype1 <- if (isid1 == TRUE) {
    "1"
  } else {
    "m"
  }

  mtype2 <- if (isid2 == TRUE) {
    "1"
  } else {
    "m"
  }


  mtype <- paste(mtype1, mtype2, sep = ":")

  if (mtype == "m:m") {
    cli::cli_abort("Auxiliary files shouldn't have `m:m` relationship")
  }


  mdata <- joyn::joyn(aux_data1,
                      aux_data2,
                      by = int_key,
                      match_type = mtype,
                      keep = merge_type,
                      keep_common_vars = commn_vars)

  attr(mdata, "aux_key", union(attr(aux_data1, "aux_key"),
                               attr(aux_data2, "aux_key")))

  setattr(mdata, "aux_name", paste(measure1, measure2, sep = "_"))

  return(mdata)

}
