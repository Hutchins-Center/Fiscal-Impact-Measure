# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
source('src/functions.R')
source('src/packages.R')

# CBO projections -----------------------------------------------------------
# Economic --------------------------------------------------------------------------------------------
# Quarterly 
econ <-
  read_xlsx(here('data/raw/cbo/cbo_econ_proj_quarterly.xlsx')) %>%
  mutate(date =  gsub("12/30/", "12/31/", date)) %>%
  mutate(date = as.Date(date))

comp = colnames(econ)[!colnames(econ) %in% "date"]
# Annual 
econ_a <-
  read_xlsx(here('data/raw/cbo/cbo_econ_proj_annual.xlsx')) %>%
  mutate(date = as.Date(paste0(calendar_date, "-12-31"), 
                        f = "%Y-%m-%d")) %>%
  filter(date > Sys.Date()) # keep annuals for current calendar year

# Budget ----------------------------------------------------------------------------------------------
budg <-
  read_xlsx(here('data/raw/cbo/cbo_budget_nipas_proj_annual.xlsx'))

# Federal Medical Assistance Percentage (FMAP) --------------------------------------------------------------------
fmap <-
  read_xlsx(here('data/raw/nhe_fmap.xlsx'))

# Haver data ------------------------------------------------------------------------------------------------------

# Load U.S. national accounts and economic statistics data into the Global Environment
haver_data_path <-
  here('data/raw/haver/')
haver_data_names <- 
  haver_data_path  %>%
  list.files() %>%
  .[str_detect(., ".xlsx")]

# Load raw Haver data into global environment
haver_data_names %>%  
  purrr::map(function(file_name){ # iterate through each file name
    assign(x = str_remove(file_name, ".xlsx"), # Remove file extension ".csv"
           value = read_xlsx(paste0(haver_data_path, file_name)) %>%
             mutate(date = as.Date(date)),
           envir = .GlobalEnv)
  }) 
# Merge quarterly and annual data 
# change hist and aa to haver quarterly and annual
hist <-
  left_join(national_accounts,
            economic_statistics,
            by = "date") 
