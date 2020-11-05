# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
source('src/functions.R')
source('src/packages.R')

# We will translate annual levels into quarterly values 
# by imputing them using a 4-quarter moving average


# Haver data ------------------------------------------------------------------------------------------------------

# Load U.S. national accounts and economic statistics data into the Global Environment
haver_data_path <-
  here('data/raw/haver/')
haver_data_names <- 
  haver_data_path  %>%
  list.files() %>%
  .[str_detect(., ".xlsx")]

# Load raw Haver data into global environment
START_DATE <- '1999-12-31'
haver_data_names %>%  
  purrr::map(function(file_name){ # iterate through each file name
    assign(x = str_remove(file_name, ".xlsx"), # Remove file extension ".csv"
           value = read_xlsx(paste0(haver_data_path, file_name)) %>%
             filter(date > START_DATE) %>%
             mutate(date = as.Date(date)),
           envir = .GlobalEnv)
  }) 
# Merge quarterly and annual data 
hist <-
  left_join(national_accounts_quarterly,
        economic_quarterly,
        by = "date") 

aa <-
  left_join(national_accounts_annual,
            economic_annual,
            by = "date") %>%
  # Use FHFA  purchase only housing price index since that's what CBO forecasts
  mutate(hpx = usphpi)
# CBO data -----------------------------------------------------------
cbo_data_path <-
  here('data/raw/cbo/')

cbo_data_names <- 
  cbo_data_path %>%
  list.files() %>%
  .[str_detect(., '.xlsx')]

 cbo_data_names %>%
  purrr::map(
    function(file_name){
      read_xlsx(paste0(cbo_data_path, file_name))
    }
  ) %>%
  setNames(., c('budg',
                'econ_a',
                'econ')
           ) %>%
   map(
     ~mutate(., date = as.Date(date))
   )
  
  
# Load raw quarterly CBO economic projections 
econ = read.csv("./data/raw/cbo_econ_proj_quarterly.csv", stringsAsFactors = F) %>% 
  as_tibble()
econ$date = gsub("12/30/", "12/31/", econ$date)
econ$date = as.Date(econ$date, f = "%m/%d/%Y")
comp = colnames(econ)[!colnames(econ) %in% "date"]

# pull annual CBO economic projections data
econ_a = read.csv("./data/raw/cbo_econ_proj_annual.csv", stringsAsFactors = F)
econ_a$date = as.Date(paste0(econ_a$calendar_date, "-12-31"), 
                      f = "%Y-%m-%d")
econ_a = econ_a[econ_a$date > Sys.Date(), ]  # keep annuals for current calendar year

# pull annual CBO budget projections, 'as they appear in the
# NIPAS' budg =
# read.csv('data/cbo_budget_nipas_proj_annual.csv',
# stringsAsFactors = F)
budg = read.csv("./data/raw/cbo_budget_nipas_proj_annual_new.csv", 
                stringsAsFactors = F) %>% as_tibble()

# pull annual FMAPS data, which come from CMS.gov, NHE by
# type of service and source of funds. Annual data, later
# translated to quarterly just as we do with the budget data.
fmap = read.csv("./data/raw/nhe_fmap.csv", stringsAsFactors = F)

