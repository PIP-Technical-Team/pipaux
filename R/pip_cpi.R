pip_cpi <- function(action = "load",
                    maindir = "//w1wbgencifs01/pip/PIP-Data/",
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
    #
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

pip_cpi_update <- function(cpidir,
                           dlwdir = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/"
                           ){

  # check for last version in dlw
  cpidlw <- paste0(dlwdir, "Support_2005_CPI_v04_M/Data/Stata/Final_CPI_PPP_to_be_used.dta")

  cpi    <- haven::read_dta(cpidlw)

  # Note clean CPI data file and then create datasignature
  ds_dlw <- digest::digest(cpi,  algo = "xxhash64") # Data signature of file

  # check signature of current fst file
  ds_production_path <- paste0(cpidir, "cpi_datasignature.txt")  # data signature in production


  if (file.exists(ds_production_path)) {
    # read data signature in production
    ds_production <- readr::read_lines(ds_production_path)

  } else {
    ds_production <- "0000" # fake signature
  }

  #--------- if Signature from dlw is differnet from the one in production ---------

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
    print("Data signature was not found, so cpi.fst has been updated")
    return(invisible(TRUE))
  } else {
    print("Data signature is up to date. No update performed")
    return(invisible(FALSE))
  }
}

#--------- Find latest dlw ---------

