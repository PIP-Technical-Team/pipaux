
#--------- To delete. Just for testing purposes ---------

cpidlw_load <- function() {
  dlwdir <- "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/Support_2005_CPI_v04_M/Data/Stata/"

  cpid <<- haven::read_dta(paste0(dlwdir, "Final_CPI_PPP_to_be_used.dta"))
  pfwd <<- haven::read_dta(paste0(dlwdir, "Survey_price_framework.dta"))


}


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

#--------- Last item in character vector separator ---------

last_item <- function(x, word = "and") {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], word, x[2])
  }
  else {
    y <- c(x[1:lx-1], paste(word, x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}






