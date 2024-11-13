#' Attache key values into auxiliary file
#'
#' @param aux_file auxiliary file
#'
#' @return data.table with key values
#' @export
#'
#' @examples
aux_data <- function(aux_file){

  # list of possible auxiliary keys --------------------------------------------
  keycolsg0 <- c("country_code") # countries, country_list
  keycolsg1 <- c("country_code", "surveyid_year") # maddison, weo, npl, income_group
  keycolsg2 <- c("country_code", "surveyid_year", "reporting_level")  # gdp, pce, pop
  keycolsg3 <- c("country_code", "surveyid_year", "reporting_level") # gdm
  keycolsg4 <- c("country_code", "surveyid_year", "survey_acronym", "reporting_level") # cpi "survey_acronym"
  keypfw <- c("country_code", "surveyid_year", "year", "survey_acronym",
              "reporting_level")

  # list of all the auxiliary files
  aux_file_names <- c("pfw", "cpi", "gdp", "gdm", "pce", "pop", "ppp", "maddison",
                      "weo", "npl", "countries", "country_list", "regions",
                      "income_groups", "metadata")

  if (deparse(substitute(aux_file)) %chin% aux_file_names) {

    # pfw  ---------------------------------------------------------------------
    if (deparse(substitute(aux_file)) == "pfw"){

      setkeyv(pfw,
              c("country_code", "survey_year", "survey_acronym", "cpi_domain"))

      # generate a dataset that can be used to add reporting_level variable to pfw data
      pfw_key <- pip_pfw_key()

      aux_file <-  pfw_key[aux_file] |>
        setkeyv(keypfw)
    } else if (deparse(substitute(aux_file)) == "ppp"){

      # ppp --------------------------------------------------------------------
      # filter ppp based on defualt ppp value
      aux_file <- ppp[ppp_default == TRUE,
                      .(country_code, ppp_year, ppp, ppp_data_level)]

      setnames(aux_file, "ppp_data_level", "reporting_level",
               skip_absent=TRUE)

      setkeyv(aux_file, c("country_code", "reporting_level"))

    } else if (deparse(substitute(aux_file)) == "cpi"){

      # cpi --------------------------------------------------------------------
      # rename two variables cpi_year to surveyid_year and cpi_data_level to reporting_level
      aux_file <- aux_file |>
        setnames(c("cpi_year", "cpi_data_level"),
                 c("surveyid_year", "reporting_level"),
                 skip_absent=TRUE)

      setkeyv(aux_file, c("country_code", "surveyid_year", "survey_acronym", "reporting_level")) #keycolsg4)

    } else if (deparse(substitute(aux_file)) == "gdm"){

      # gdm --------------------------------------------------------------------
      aux_file <- aux_file |>
        setnames("pop_daaux_file ta_level", "reporting_level",
                 skip_absent=TRUE)

      setkeyv(aux_file, keycolsg3)

    } else if (deparse(substitute(aux_file)) == "npl"){

      # npl --------------------------------------------------------------------
      aux_file <- aux_file |>
        setnames("reporting_year", "surveyid_year",
                 skip_absent=TRUE)

      setkeyv(aux_file, keycolsg1)

    } else if (deparse(substitute(aux_file)) == "income_groups"){

      # income_groups ------------------------------------------------------------
      # rename year_data into surveyid_year
      aux_file <- aux_file |>
        setnames("year_data", "surveyid_year",
                 skip_absent=TRUE)

      setkeyv(aux_file, keycolsg1)

    } else if (deparse(substitute(aux_file)) == "countries"){

      # countries ----------------------------------------------------------------
      setkeyv(aux_file, keycolsg0)

    } else if (deparse(substitute(aux_file)) == "country_list"){

      # country_list--------------------------------------------------------------
      setkeyv(aux_file, keycolsg0)

    } else if (deparse(substitute(aux_file)) == "metadata"){

      # metadata -----------------------------------------------------------------
      setkeyv(aux_file, keycolsg5)

    } else if (deparse(substitute(aux_file)) %chin% c("maddison", "weo")){

      # auxiliary datasets - group 1 (maddison and weo) --------------------------
      aux_file |>
        setnames("year", "surveyid_year",
                 skip_absent=TRUE)

      setkeyv(aux_file, keycolsg1)

    } else if (deparse(substitute(aux_file)) %chin% c("gdp", "pop", "pce")){

      # auxiliary datasets - group 2 (gdp, pop, pce) ---------------------------
      aux_data_level <- paste0(deparse(substitute(aux_file)), "_data_level")

      aux_file |>
        setnames(c(aux_data_level, "year"),
                 c("reporting_level", "surveyid_year"),
                 skip_absent=TRUE)

      setkeyv(aux_file, keycolsg2)

    }

    return(aux_file)

  } else {

    return(aux_file)

  }

}

