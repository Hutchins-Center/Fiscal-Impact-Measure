# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
source('src/functions.R')
source('src/packages.R')

data1 <- read_xlsx("data/raw/data1.xlsx") %>% 
  mutate(date = as.Date(date, f = "%m/%d/%y")) %>% 
  as_tibble()
data2 <- read_xlsx("data/raw/data2.xlsx") %>% 
  mutate(date = as.Date(date, f = "%m/%d/%y")) %>% 
  as_tibble()

hist <-
  merge(data1, data2,
        by = "date") %>% 
  as_tibble()


data3 <- read_xlsx("data/raw/data3.xlsx")
data4 <- read_xlsx("data/raw/data4.xlsx") 

aa <-
  merge(data3, data4,
        by = "date")
aa$hpx = aa$usphpi  #house price index of choice is the FHFA purchase only index, 1991 = 100, since that's what CBO forecasts



# Pull CBO data -----------------------------------------------------------


# pull quarterly CBO economic projections data
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

