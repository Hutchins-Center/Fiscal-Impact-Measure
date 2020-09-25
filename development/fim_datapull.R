library(reshape2)
library(plyr)
library(zoo)
library(quantmod)
library(rmarkdown)
library(TTR)
library(Haver)
library(data.table)
library(dplyr)

# define some helper functions
# function to pull haver data 
pull_data = function(series, database, start.date, frequency = "quarterly"){
  q = haver.data(series, database, eop.dates = T, start = as.Date(start.date, f = "%m-%d-%Y"))
  q = data.frame(date = as.Date(rownames(q)), q)
  
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

# NOTE: All quarterly values are in seasonally-adjusted, annual rates (SAAR), billions of dollars. Annual values are in annual rates, billions of dollars. We will translate annual levels into quarterly values by imputing them to each of the four quarters in the year and taking the 4-quarter moving average. 

# pull quarterly BEA NIPAs data
series1 = c("GDP", "C","CH","GDPH","JC", "JGDP", "JGF", "JGS", "PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "YPTU", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "GRCSI", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET")
data1 = pull_data(series1, "usna", start.date = "01-01-1970")
metadata1 = cbind(haver.metadata(series1, "usna")$code, haver.metadata(series1, "usna")$descriptor) # use this for reference
data2 = pull_data(c("PCW", "GDPPOTHQ","GDPPOTQ", "RECESSQ"), "usecon", "01-01-1970")
hist = merge(data1, data2, by = "date")

# pull annual BEA NIPAs data, for regressions for state and local tax revenues
series3 = c("GDP", "C","CH","GDPH","JC", "JGDP", "JGF", "JGS", "PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "GRCSI", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET", "YP")
series4 = c("USPHPI", "CASUSXAM", "FNMFHFA", "GDPPOT", "GDPPOTH")
data3 = pull_data(series3, "usna", start = as.Date("1970-01-01"), frequency = "annual")
data4 = pull_data(series4, "usecon", start = as.Date("1970-01-01"), frequency = "annual")
aa = merge(data3, data4, by = "date")
aa$hpx = aa$usphpi #house price index of choice is the FHFA purchase only index, 1991 = 100, since that's what CBO forecasts

# pull quarterly CBO economic projections data
econ = read.csv('cbo_econ_proj_quarterly.csv', stringsAsFactors = F)
econ$date = gsub("12/30/", "12/31/", econ$date)
econ$date = as.Date(econ$date, f = "%m/%d/%Y")
comp = colnames(econ)[!colnames(econ) %in% "date"]

# pull annual CBO economic projections data
econ_a = read.csv('cbo_econ_proj_annual.csv', stringsAsFactors = F)
econ_a$date = as.Date(paste0(econ_a$calendar_date, "-12-31"), f="%Y-%m-%d")
econ_a = econ_a[econ_a$date > Sys.Date(),] # keep annuals for current calendar year

# pull annual CBO budget projections, "as they appear in the NIPAS"
budg = read.csv('cbo_budget_nipas_proj_annual.csv', stringsAsFactors = F)

# pull annual FMAPS data, which come from CMS.gov, NHE by type of service and source of funds. Annual data, later translated to quarterly just as we do with the budget data.  
fmap = read.csv('nhe_fmap.csv', stringsAsFactors = F)
# fmap$fshare = fmap$gf_medicaid / (fmap$gf_medicaid + fmap$gs_medicaid)

