# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
source('src/packages.R')
library(fim)
# CBO projections -----------------------------------------------------------
# Economic --------------------------------------------------------------------------------------------
# Quarterly 
load_economic_projections <- function(){
  read_xlsx(here('data/raw/cbo/cbo_econ_proj_quarterly.xlsx')) %>%
  mutate(date = as_date(date))
}
load_budjet_projections <- function(){
  read_xlsx(here('data/raw/cbo/cbo_budget_nipas_proj_annual.xlsx')) %>%
    as_tsibble(index = fy)
}
get_haver_data <- function(){
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
      assign(x = str_remove(file_name, ".xlsx"), 
             value = read_xlsx(paste0(haver_data_path,"/", file_name), na = 'NA') %>%
               mutate(date = as.Date(date)),
             envir = .GlobalEnv)
    }) 
  # Merge quarterly and annual data 
  # change hist and aa to haver quarterly and annual
  left_join(national_accounts,
            economic_statistics,
            by = "date") 
}
econ <- load_economic_projections()
budg <- load_budget_projections()
fmap <- read_xlsx(here('data/raw/nhe_fmap.xlsx'))
hist <- get_haver_data()





