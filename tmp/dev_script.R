rm(list = ls())
library(data.table)
library(magrittr)
#popdir <- '//wbntpcifs/01.PovcalNet/03.QA/03.Population/data/'


pop_dir <- 'P:/01.PovcalNet/03.QA/03.Population/data/'
pop_special <- 'population_missing_2020-12-01.xlsx'
pop_files <- list.files(pop_dir, pattern = 'population_country')
pop_latest <- pop_files %>%
  gsub('population_country_|.xlsx', '', .) %>%
  as.POSIXlt() %>%
  max() %>%
  as.character() %>%
  sprintf('population_country_%s.xlsx', .)
pop_path <- paste0(pop_dir, pop_latest)
pop_special_path <- paste0(pop_dir, pop_special)

pop <- readxl::read_xlsx(pop_path, sheet = 'Sheet1')
names(pop)[1:4] <- as.character(pop[2, 1:4])
pop <- pop[-c(1:2),]
year_vars <- names(pop[,6:ncol(pop)])
pop$Series_Name <- NULL
pop$Time_Name <- NULL

pop_long <- pop %>%
  data.table::setDT() %>%
  data.table::melt(id.vars = c('Country', 'Series'),
                   measure.vars = year_vars,
                   variable.name = 'Year',
                   value.name = 'Population')
pop_long$Year <- as.character(pop_long$Year)
pop_long$Population <- as.numeric(pop_long$Population)

pop_special <- readxl::read_xlsx(pop_special_path, sheet = 'Long')
pop_special <- pop_special[c('Country', 'Series', 'Time', 'Data')]
names(pop_special)[3:4] <- c('Year', 'Population')
pop_special$Year <- sub('YR', '', pop_special$Year)

pop <- rbind(pop_long, pop_special)
pop$Series <- NULL
#setDT(pop)

# data level
pop[,
    pop_data_level :=
      fcase(
        grepl("POP", Series), 2,
        grepl("RUR", Series), 0,
        grepl("URB", Series), 1
      )
]
setnames(pop,
         old = c("Country", "Year", "Population"),
         new = c("country_code", "year", "pop"))


x$Year


df_long$Population

#names(df_special) <- c('Country', )

readxl:.pop_special_path


# Find latest masterfile
m_latest <- m_files %>%
  gsub('Master_|.xlsx', '', .) %>%
  as.POSIXlt(format = '%Y%m%d%H%M%S') %>%
  max() %>%
  as.character() %>%
  gsub('-|:| ', '', .) %>%
  sprintf('Master_%s.xlsx', .)
master_path <- sprintf('%s/%s', input_dir, m_latest)
rm(m_files, m_latest)
