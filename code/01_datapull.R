# Packages ----------------------------------------------------------------

mPackages <- installed.packages()
# Details of installed packages
stInstalled <- rownames( mPackages )
# Isolate thep package names
stRequired <- c('tidyverse','stringr','reshape2', 'zoo', 'quantmod', 'rmarkdown', 'TTR',
   'data.table', 'lubridate', 'Hmisc', 'ggplot2', 'readxl')
#  The required packages
for ( stName in stRequired ){
  if ( !( stName %in% stInstalled ) ){
    cat('****************** Installing ', stName, '****************** \n')
    install.packages( stName )
  }
  library( stName, character.only=TRUE )
}

# since Haver is a proprietary package, load it separately
if (!('Haver' %in% stInstalled) ){
  install.packages('Haver', repos = "http://www.haver.com/r/", type = "win.binary")
}
library(Haver)
# functions ---------------------------------------------------------------

# define some helper functions
# function to pull haver data 
#' Title
#'
#' @param series variables to search for in Haver
#' @param database Haver name
#' @param start.date 
#' @param frequency can be annual or quarterly. Default is quarterly
#'
#' @return data frame
pull_data <- function(series, names, database, start.date, frequency = "quarterly"){
  q <- haver.data(series, database, eop.dates = T, start = as.Date(start.date, f = "%m-%d-%Y"))
  q <- data.frame(q) %>% setnames(., names)
  q <- data.frame(date = as.Date(rownames(q)), q)
  
  for (j in 2:ncol(q)) {
    for (k in 4:nrow(q)) {
      if (is.na(q[k,j])){
        q[k,j]=mean(q[c(k-1,k-2,k-3),j]) # if the data is missing on unreported, use the 3-qtr moving average
        
      }
    }
  }
  q
}

# function to calculate quarterly annualized growth rates
#' Title
#'
#' @param x is a time series object
#'
#' @return annualized quarterly growth rate
#' @export
#'
#' @examples
q_a = function(x){
  j=c()
  for(i in 2:length(x)){
    j[i] = (((x[i]/x[i-1])^4)-1)*100
  }
  j[1] = 0
  j
}

# function to calculate quarter-over-quarter growth rate
q_g = function(x){
  j=c()
  for(i in 2:length(x)){
    j[i] = (((x[i]/x[i-1]))-1)
  }
  j[1] = j[2]
  j
}

#' Title
#' Read all sheets in excel workbook
#' @param filename 
#' @param tibble 
#'
#' @return
#' @export
#'
#' @examples
read_excel_allsheets <- function(filename, tibble = TRUE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x 
}
# Pull Data ---------------------------------------------------------------

# NOTE: All quarterly values are in seasonally-adjusted, annual rates (SAAR), billions of dollars. 
# Annual values are in annual rates, billions of dollars. We will translate annual levels into quarterly values 
# by imputing them to each of the four quarters in the year and taking the 4-quarter moving average. 
haverNames <- read_excel_allsheets(
  "data/processing/haver_names.xlsx"
)


# pull quarterly BEA NIPAs data
usna_quarterly <- pull_data(haverNames[["National Accounts Quarterly"]]$code, 
          haverNames[["National Accounts Quarterly"]]$reference,
          "usna", start.date = "01-01-1970")
# pull quarterly data from US Economic Statistics Database
usecon_quarterly <- pull_data(haverNames[['USECON Quarterly']]$code,
                      names = haverNames[['USECON Quarterly']]$reference,
                      "usecon", "01-01-1970")

historical_quarterly <- merge(usna_quarterly, usecon_quarterly, by = "date")

# pull annual BEA NIPAs data, for regressions for state and local tax revenues
usna_annual <- pull_data(haverNames[["National Accounts Annual"]]$code,
                         names = haverNames[['National Accounts Annual']]$reference,
                         database = "usna", start = "01-01-1970", frequency = "annual")


series4 = c("USPHPI", "CASUSXAM","GDPPOT", "GDPPOTH")
data4 = pull_data(series4, "usecon", start = as.Date("1970-01-01"), frequency = "annual")
aa = merge(data3, data4, by = "date")
aa$hpx = aa$usphpi #house price index of choice is the FHFA purchase only index, 1991 = 100, since that's what CBO forecasts

# pull quarterly CBO economic projections data
econ = read.csv('./data/cbo_econ_proj_quarterly.csv', stringsAsFactors = F)
econ$date = gsub("12/30/", "12/31/", econ$date)
econ$date = as.Date(econ$date, f = "%m/%d/%Y")
comp = colnames(econ)[!colnames(econ) %in% "date"]

# pull annual CBO economic projections data
econ_a = read.csv('./data/cbo_econ_proj_annual.csv', stringsAsFactors = F)
econ_a$date = as.Date(paste0(econ_a$calendar_date, "-12-31"), f="%Y-%m-%d")
econ_a = econ_a[econ_a$date > Sys.Date(),] # keep annuals for current calendar year

# pull annual CBO budget projections, "as they appear in the NIPAS"
# budg = read.csv('data/cbo_budget_nipas_proj_annual.csv', stringsAsFactors = F)
budg = read.csv('./data/cbo_budget_nipas_proj_annual_new.csv', stringsAsFactors = F)

# pull annual FMAPS data, which come from CMS.gov, NHE by type of service and source of funds. Annual data, later translated to quarterly just as we do with the budget data.  
fmap = read.csv('./data/nhe_fmap.csv', stringsAsFactors = F)
