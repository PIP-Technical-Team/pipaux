fetch_wdi_gdp <- function(url){

}

madd <- pip_maddison("load", maindir = maindir)
weo <- pip_gdp_weo("load", maindir = maindir)
wgdp <- wbstats::wb_data(indicator = "NY.GDP.PCAP.KD", lang = "en")
sna <- readxl::read_xlsx(sprintf("%s_aux/sna/NAS special_2021-01-14.xlsx", maindir))
cl <- pip_country_list("load", maindir = maindir)
