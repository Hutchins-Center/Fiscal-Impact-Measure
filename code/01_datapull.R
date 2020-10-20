# Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
source('src/functions.R')
source('src/packages.R')

# Pull Data
# ---------------------------------------------------------------

# We will translate annual levels into quarterly values 
# by imputing them to each of the four quarter in the year
# and taking the 4-quarter moving average

# All quarterly values are in seasonally-adjusted,
# annual rates (SAAR), billions of dollars. 

# Annual values are in annual rates, billions of dollars. 


# pull quarterly BEA NIPAs data
names_usna <- read_excel("data/auxilliary/haver_names.xlsx")

series1 = c("GDP", "C", "CH", "GDPH", "JC", "JGDP", "JGF", "JGS", 
  "JGSE", "JGSI", "PTGH", "PTGSH", "PTGFH", "YPTMR", "YPTMD", 
  "YPTU", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GRCSI", 
  "GDPH", "DC", "PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", 
  "\tGFRPT", "GFRPRI", "GFRCP", "GFRS", "GFRPT", "\tGFRPRI", 
  "\tGFRCP", "\tGFRS", "\tGFTFP", "\tGFEG", "\tGSRPT", "\tGSRPRI", 
  "\tGSRCP", "\tGSRS", "\tGSTFP", "\tGSET", "GFEGHHX", "GFEGHDX", 
  "GFEIGX", "GFSUB", "GSSUB", "GSUB", " GFTFBUSX")
# data1 = pull_data(names_usna$code, "usna", start.date = "01-01-1970")
# START <- "01-01-1970"
# usna <- haver.data(names_usna$code, database = "usna", eop.dates = T, 
#   start = as.Date(START, f = "%m-%d-%Y"))
# colnames(data1) <- names_usna$reference[match(colnames(usna), 
#   names_usna$code)]
# 
# metadata1 = cbind(haver.metadata(series1, "usna")$code, haver.metadata(series1, 
#   "usna")$descriptor)  # use this for reference
# data2 = pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ"), 
#   "usecon", "01-01-1970")

data1 <- read_csv("data/raw/data1.csv") %>% 
  mutate(date = as.Date(date, f = "%m/%d/%y")) %>% 
  as_tibble()
data2 <- read_csv("data/raw/data2.csv") %>% 
  mutate(date = as.Date(date, f = "%m/%d/%y")) %>% 
  select(-X1) %>% 
  as_tibble()
hist = merge(data1, data2, by = "date") %>% as_tibble()

# pull annual BEA NIPAs data, for regressions for state and
# local tax revenues
series3 = c("GDP", "C", "CH", "GDPH", "JC", "JGDP", "JGF", "JGS", 
  "PTGH", "PTGSH", "PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", 
  "YPTX", "YTPI", "YCTLG", "G", "GRCSI", "GDPH", "DC", "PTGFH", 
  "PTGSH", "GF", "GS", "GFH", "GSH", "\tGFRPT", "GFRPRI", "GFRCP", 
  "GFRS", "GFRPT", "\tGFRPRI", "\tGFRCP", "\tGFRS", "\tGFTFP", 
  "\tGFEG", "\tGSRPT", "\tGSRPRI", "\tGSRCP", "\tGSRS", "\tGSTFP", 
  "\tGSET", "YP", "GFSUB", "GSSUB")
series4 = c("USPHPI", "CASUSXAM", "GDPPOT", "GDPPOTH")
# data3 = pull_data(series3, "usna", start = as.Date("1970-01-01"), 
#   frequency = "annual")
# data4 = pull_data(series4, "usecon", start = as.Date("1970-01-01"), 
#   frequency = "annual")

data3 <- read_csv("data/raw/data3.csv") %>% 
  select(-X1)
  as_tibble()
data4 <- read_csv("data/raw/data4.csv") %>% 
  select(-X1)
  as_tibble()
aa = merge(data3, data4, by = "date") %>% as_tibble()
aa$hpx = aa$usphpi  #house price index of choice is the FHFA purchase only index, 1991 = 100, since that's what CBO forecasts

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
