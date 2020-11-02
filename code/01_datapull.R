# 0 Setup ----------------------------------------------------------------------


# * 0.1 Packages ---------------------------------------------------------------
mPackages <- installed.packages()
# Details of installed packages
stInstalled <- rownames(mPackages)
# Isolate thep package names
stRequired <- c('tidyverse','stringr','reshape2', 'zoo', 'quantmod',
                'rmarkdown', 'TTR','data.table', 'lubridate',
                'Hmisc', 'ggplot2', 'readxl')
#  The required packages
for ( stName in stRequired ){
  if ( !(stName %in% stInstalled ) ){
    cat('****************** Installing ', stName, '****************** \n')
    install.packages( stName )
  }
  library( stName, character.only=TRUE )
}
# since Haver is a proprietary package, load it separately
if (!('Haver' %in% stInstalled) ){
  install.packages('Haver', 
                   repos = "http://www.haver.com/r/",
                   type = "win.binary")
}
library(Haver)
# * 0.2 functions ---------------------------------------------------------------

#' Title
#' Haver pull_data()
#' @param series variables to search for in Haver
#' @param database Haver name
#' @param start.date 01-01-1970 by default
#' @param frequency can be annual or quarterly. Default is quarterly
#'
#' @return data frame
pull_data <- function(series, database,
                      start.date = "01-01-1970", 
                      frequency = "quarterly"){
  q <- haver.data(series, database, eop.dates = T, start = as.Date(start.date, f = "%m-%d-%Y"))
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


#' Title
#' lookup_rename
#' @param df data to rename
#' @param column_lookup conversion keys
#'
#' @return variables renamed according to index
#' @export
#'
#' @examples
lookup_rename <- function(df, column_lookup) {
  df2 <- df
  # using Gregor's answer (https://stackoverflow.com/a/43742442/3927208)
  names(df2) = column_lookup$reference[match(names(df), column_lookup$code)]
  df2
}
# 1 Data -----------------------------------------------------------------------
# NOTE: All quarterly values are in seasonally-adjusted, 
# annual rates (SAAR), billions of dollars. 
# Annual values are in annual rates, billions of dollars. 
# We will translate annual levels into quarterly values 
# by imputing them to each of the four quarters in the year and
# taking the 4-quarter moving average. 
haverNames <- read_excel_allsheets(
  "data/processing/haver_names.xlsx"
)
lookupNames <- haverNames[['lookup']]
# * 1.1 Historical -------------------------------------------------------------
# * * 1.1.1 Quarterly ----------------------------------------------------------
# BEA NIPAs data
usna_quarterly <- pull_data(haverNames[["National Accounts Quarterly"]]$code, 
          "usna") %>%
  lookup_rename(lookupNames)
# US Economic Statistics Database
usecon_quarterly <- pull_data(haverNames[['USECON Quarterly']]$code,
                      "usecon") %>%
  lookup_rename(lookupNames)

historical_quarterly <- 
  merge(usna_quarterly, usecon_quarterly, by = "date")
# * * 1.1.2 Annual -------------------------------------------------------------
# pull annual BEA NIPAs data, for regressions for state and local tax revenues
usna_annual <- pull_data(haverNames[["National Accounts Annual"]]$code,
                         database = "usna",
                         frequency = "annual") %>%
  lookup_rename(lookupNames)

usecon_annual <- pull_data(haverNames[['USECON Annual']]$code, "usecon", 
                           frequency = "annual") %>% 
  lookup_rename(lookupNames)

historical_annual <- merge(usna_annual, usecon_annual, by = "date")

# * 1.2 Projections -------------------------------------------------------


# * * 1.2.1 Quarterly -----------------------------------------------------

# pull quarterly CBO economic projections data


simple_function <- function(dataset, filter_col, mean_col, value){
  
  filter_col <- enquo(filter_col)
  mean_col <- enquo(mean_col)
  mean_name <- paste0("mean_", as_label(mean_col))
  
  dataset %>%
    filter((!!filter_col) == value) %>%
    summarise(!!(mean_name) := mean((!!mean_col)))
}


deflator <- function(df, x){
  x <- enquo(x)
  newName <- paste0("j", as_label(x))
  realX <- rlang::sym(paste0(as_label(x), "h"))
  
    mutate(df, {{newName}} := (!!x) / (!!realX))
}



econ_projections_quarterly <- read.csv("data/cbo_econ_proj_quarterly.csv",
                                       stringsAsFactors = FALSE) %>%
  mutate(date =  gsub("12/30","12/31", date)) %>%
  mutate(date = as.Date(date, f = "%m/%d/%Y")) %>%
  deflator(gf) %>% deflator(gs) %>% deflator(c) %>%
  lookup_rename(lookupNames)

# * * 1.2.2 Annual --------------------------------------------------------
# pull annual CBO economic projections data
econ_projections_annual <- 
  read.csv("data/cbo_econ_proj_annual.csv",
            stringsAsFactors = FALSE) %>%
  rename(date = calendar_date) %>%
  mutate(date = as.Date(paste0(date, "-12-31"), f = "%Y-%m-%d")) %>%
  filter(date > Sys.Date()) %>% #keep annuals for current calendar year
  lookup_rename(lookupNames)

# CBO budget projections "as they appear in the NIPAS"
# Later translated into quarterly data
budget_projections_annual <-
  read.csv('./data/cbo_budget_nipas_proj_annual_new.csv', stringsAsFactors = F)

# NHE by type of service and source of funds.
# Later translated to quarterly 
fmap <-
  read.csv('data/nhe_fmap.csv', stringsAsFactors = F)



