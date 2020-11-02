
# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
source('src/functions.R')
source('src/packages.R')

# 0.1 Pull Raw Data---------------------------------------------------------------

# We will translate annual levels into quarterly values 
# by imputing them to each of the four quarter in the year
# and taking the 4-quarter moving average

# All quarterly values are in seasonally-adjusted,
# annual rates (SAAR), billions of dollars. 

# Annual values are in annual rates, billions of dollars. 


# pull quarterly BEA NIPAs data
names_usna <- read_excel("data/auxilliary/haver_names.xlsx")
# series1 = c("GDP", "C", "CH", "GDPH", "JC", "JGDP", "JGF", "JGS", 
#   "JGSE", "JGSI", "PTGH", "PTGSH", "PTGFH", "YPTMR", "YPTMD", 
#   "YPTU", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GRCSI", 
#   "GDPH", "DC", "PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", 
#   "\tGFRPT", "GFRPRI", "GFRCP", "GFRS", "GFRPT", "\tGFRPRI", 
#   "\tGFRCP", "\tGFRS", "\tGFTFP", "\tGFEG", "\tGSRPT", "\tGSRPRI", 
#   "\tGSRCP", "\tGSRS", "\tGSTFP", "\tGSET", "GFEGHHX", "GFEGHDX", 
#   "GFEIGX", "GFSUB", "GSSUB", "GSUB", " GFTFBUSX")

data1 = pull_data(names_usna$code, "usna", start.date = "01-01-1970") %>%
  as_tibble()
START <- "01-01-1970"
usna <- haver.data(names_usna$code, database = "usna", eop.dates = T,
                   start = as.Date(START, f = "%m-%d-%Y"))
# colnames(data1) <- names_usna$reference[match(colnames(usna),
#                                               names_usna$code)]
# 
# metadata1 = cbind(haver.metadata(series1, "usna")$code, haver.metadata(series1, 
#   "usna")$descriptor)  # use this for reference
data2 = pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ"), 
                  "usecon", "01-01-1970")


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
data3 = pull_data(series3, "usna", start = as.Date("1970-01-01"), 
                  frequency = "annual") %>%
  as_tibble()
data4 = pull_data(series4, "usecon", start = as.Date("1970-01-01"), 
                  frequency = "annual") %>%
  as_tibble()


# Write csv to current month's folder
haver_raw <- 
  list(usna = data1, data2 = data2,
       data3 = data3, data4 = data4)

# Write haver data to raw folder ------------------------------------------

write_xlsx(data1, "data/raw/data1.xlsx")
write_xlsx(data2, "data/raw/data2.xlsx")
write_xlsx(data3, "data/raw/data3.xlsx")
write_xlsx(data4, "data/raw/data4.xlsx")

