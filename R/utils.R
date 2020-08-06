
#--------- common value ---------

pip_aux_values <- function(){
  r <- list(
    dlwdir  = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
    maindir = "//w1wbgencifs01/pip/PIP-Data/"
  )
  return(r)
}


#--------- Find latest dlw directory ---------

latest_dlw_dir <- function(dlwdir){
  dlw_dirs <- dir(dlwdir)
  latest   <- max(dlw_dirs)
  return(latest)
}
