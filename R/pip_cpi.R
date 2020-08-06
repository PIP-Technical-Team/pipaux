pip_cpi <- function(action  = "load",
                    maindir = NULL,
                    dlwdir  = NULL
                    ){


  #----------------------------------------------------------
  #   conditions
  #----------------------------------------------------------

  action <- tolower(action) # convert to lower case just in case

  # Proper length
  if (length(action) != 1) {
    stop(paste0("`action` should be length 1, not ", length(action)))
  }

  # proper options
  action_options <- c("load", "update")
  if (!(action  %in% action_options)) {
    stop(cat("`action` should be one of ", action_options))
  }


  #----------------------------------------------------------
  #   define parameters
  #----------------------------------------------------------

  # Always call common values
  com_values <- pip_aux_values()

  if (is.null(maindir)) {
    maindir <- com_values$maindir
  }

  cpidir <- paste0(maindir, "_aux/cpi/")

  #----------------------------------------------------------
  #   execute selected function
  #----------------------------------------------------------

  #--------- load ---------
  if (action == "load") {
    df <- pip_cpi_load(cpidir = cpidir)
    return(df)
  }

  #--------- update ---------
  if (action == "update"){
    if (is.null(dlwdir)) {
      dlwdir <- com_values$dlwdir
    }

    pip_cpi_update(cpidir = cpidir,
                   dlwdir = dlwdir)
  }

}

#----------------------------------------------------------
#   Sub functions
#----------------------------------------------------------

#--------- load ---------
pip_cpi_load <- function(cpidir){

  # check file exists
  if(file.exists(paste0(cpidir, "cpi.fst"))){

    df <- fst::read_fst(paste0(cpidir, "cpi.fst"))

  } else {

    stop("file `cpi.fst` does not exist. check your connection or data availability")

  }
  return(df)
}

#--------- update ---------

pip_cpi_update <- function(cpidir, dlwdir){

  # check for last version in dlw
  dlwdir_l   <- latest_dlw_dir(dlwdir = dlwdir) # from utils.R
  cpidlw_dir <- paste0(dlwdir, dlwdir_l,"/Data/Stata/Final_CPI_PPP_to_be_used.dta")

  cpi        <- haven::read_dta(cpidlw_dir)

  # Note: clean CPI data file and then create datasignature
  ds_dlw <- digest::digest(cpi,  algo = "xxhash64") # Data signature of file

  # check signature of current fst file
  ds_production_path <- paste0(cpidir, "cpi_datasignature.txt")  # data signature in production


  if (file.exists(ds_production_path)) {

    # read data signature in production
    ds_production <- readr::read_lines(ds_production_path)

  } else {

    # fake signature
    ds_production <- "0000"
  }

  #--------- if Signature from dlw is different from the one in production ---------

  if (ds_dlw != ds_production) {

    # re-write cpi in production if data signature is not found
    fst::write_fst(x = cpi,
                   path = paste0(cpidir, "cpi.fst")
    )
    # Vintage
    time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones
    fst::write_fst(x = cpi,
                   path = paste0(cpidir, "_vintage/cpi_", time,".fst")
    )

    readr::write_lines(x = ds_dlw,
                       path = ds_production_path)
    print("Data signature has changed or was not found. `cpi.fst` has been updated")
    return(invisible(TRUE))
  } else {
    print("Data signature is up to date. No update performed")
    return(invisible(FALSE))
  }
}



#--------- clear cpi file ---------

pip_clean_cpi <- function(x) {
  data.table::setDT(x)

  # vars to keep
  keep_vars <- c("country_code", "year", "ref_year", "cpi2011", "ppp2011")

  # modifications to the database
  x[,
    c("country_code", "cur_adj", "ccf", "ppp2011")
    := {

      country_code <-  code
      cur_adj      <-  ifelse(is.na(cur_adj), 1, cur_adj)
      ccf          <-  1/cur_adj
      ppp2011      <-  icp2011
      list(country_code, cur_adj, ccf, ppp2011)
      }
    ]
  x <- x[
        ,
        ..keep_vars
       ]

  # Label variables
  attr(x$ccf, "label")      <- "Currency conversion factor"
  attr(x$ppp2011, "label")  <- "PPP values, 2011 round"



}
