#' Update GDM
#'
#' Update GDM data using the PovcalNet Masterfile.
#'
#' @inheritParams pip_gdm
#' @export
pip_gdm_update <- function(force = FALSE,
                           maindir = getOption("pipaux.maindir"),
                           pcndir = getOption("pipaux.pcndir"))  {

  # ---- Load needed datasets ----

  cl <- pip_country_list("load", maindir = maindir)
  pfw <- pip_pfw("load", maindir = maindir)
  inv <- fst::read_fst(paste0(maindir, '_inventory/inventory.fst'))

  # ---- Read PCN Masterfile ----

  # Get list of files
  m_files <- list.files(
    pcndir, pattern = 'Master_2021[0-9]{10}.xlsx')

  # Find latest Masterfile
  pcn_master_path <- m_files %>%
    gsub('Master_|.xlsx', '', .) %>%
    as.POSIXlt(format = '%Y%m%d%H%M%S') %>%
    max(na.rm = TRUE) %>%
    as.character() %>%
    gsub('-|:| ', '', .) %>%
    sprintf('%s/Master_%s.xlsx', pcndir, .)

  # Read SurveyMean sheet from latest Masterfile
  df <- readxl::read_xlsx(pcn_master_path, sheet = 'SurveyMean')


  # ---- Transform dataset ----

  # Select for grouped data surveys
  df <- df[grepl('[.]T0[1,2,5]$', df$DistributionFileName,
                 ignore.case = TRUE), ]

  # Select and rename columns
  df <- df[c('CountryCode', 'SurveyTime', 'DataType', 'Coverage',
             'SurveyMean_LCU', 'DistributionFileName', 'SurveyID')]
  names(df) <- c('country_code', 'survey_year',  'welfare_type',
                 'survey_coverage', 'survey_mean_lcu',
                 'pcn_source_file', 'pcn_survey_id')

  # Recode columns
  df$survey_coverage <- tolower(df$survey_coverage)
  df$welfare_type <- tolower(df$welfare_type)
  df$welfare_type <- ifelse(df$welfare_type == 'x', 'consumption', 'income')

  # Add pop_data_level column
  df$pop_data_level <-
    ifelse(!df$country_code %in% c('CHN', 'IDN', 'IND'),
           'national', df$survey_coverage)

  # Add dist and gd type columns
  df$distribution_type <-
    ifelse(df$pop_data_level == 'national',
           'group', 'aggregate')
  df$gd_type <- sub('.*[.]', '', df$pcn_source_file)


  # ---- Merge with PFW ----

  # Subset columns
  pfw <-
    pfw[, c('country_code', 'welfare_type',
            'surveyid_year', 'survey_year',
            'survey_acronym', 'inpovcal')]

  # Merge to add surveyid_year
  tmp <- pfw[, c('country_code', 'surveyid_year', 'survey_year')]
  df <- merge(df, tmp,  all.x = TRUE,
              by = c('country_code', 'survey_year'))

  # Merge to add survey_acronym and inpovcal
  df <- merge(df, pfw, all.x = TRUE,
              by = c('country_code', 'surveyid_year',
                     'survey_year', 'welfare_type'))

  # Filter to select surveys in PovcalNet
  df <- df[df$inpovcal == 1, ]
  df <- df[!is.na(df$inpovcal), ]


  # ---- Merge with inventory ----

  # Create survey_id column
  inv$survey_id <- sub('[.]dta', '', inv$filename)

  # Subset GD rows
  inv <- inv[inv$module == 'PC-GROUP',]

  # Subset columns
  inv <- inv[, c('country_code', 'surveyid_year',
                             'survey_acronym', 'survey_id')]

  # Merge to add PIP survey_id
  df <-  merge(df, inv, all.x = TRUE,
               by = c('country_code', 'surveyid_year',
                      'survey_acronym'))


  # ---- Finalize table ----

  # Select columns
  df <- df[c('country_code', 'surveyid_year', 'survey_year',
             'welfare_type', 'survey_mean_lcu',
             'distribution_type', 'gd_type',
             'pop_data_level', 'pcn_source_file',
             'pcn_survey_id', 'survey_id')]
  df$survey_id <- toupper(df$survey_id)

  # Convert LCU means to daily values
  df$survey_mean_lcu <- df$survey_mean_lcu * (12/365)

  # Convert to data.table
  dt <- data.table::as.data.table(df)

  # Sort rows
  data.table::setorder(dt, country_code, surveyid_year, pop_data_level)

  # Sort columns
  data.table::setcolorder(dt, 'survey_id')

  # Remove any non-WDI countries
  dt <- dt[country_code %in% cl$country_code]


  # ---- Save and sign ----

  measure <- "gdm"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")

  pip_sign_save(x       = dt,
                measure = measure,
                msrdir  = msrdir,
                force   = force)

}
