
# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('Haver')
library('readxl')
library('writexl')

source('src/functions.R')

# 0.1 Pull Raw Data---------------------------------------------------------------

# We yranslate annual levels into quarterly values by imputing them with the 4-quarter moving average
# Quarterly values are in seasonally-adjusted, annual rates (SAAR), billions of dollars. 
# Annual values are in annual rates, billions of dollars. 

START <- "01-01-1970"

# pull quarterly BEA NIPAs data
###### series1 #############
# series1 = c("GDP", "C", "CH", "GDPH", "JC", "JGDP", "JGF", "JGS", 
#   "JGSE", "JGSI", "PTGH", "PTGSH", "PTGFH", "YPTMR", "YPTMD", 
#   "YPTU", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GRCSI", 
#   "GDPH", "DC", "PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", 
#   "\tGFRPT", "GFRPRI", "GFRCP", "GFRS", "GFRPT", "\tGFRPRI", 
#   "\tGFRCP", "\tGFRS", "\tGFTFP", "\tGFEG", "\tGSRPT", "\tGSRPRI", 
#   "\tGSRCP", "\tGSRS", "\tGSTFP", "\tGSET", "GFEGHHX", "GFEGHDX", 
#   "GFEIGX", "GFSUB", "GSSUB", "GSUB", " GFTFBUSX")
######
names_usna <- read_excel("data/auxilliary/haver_names.xlsx")
data1 <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble()


###### SERIES 2 ####
# colnames(data1) <- names_usna$reference[match(colnames(usna),
#                                               names_usna$code)]
# 
# metadata1 = cbind(haver.metadata(series1, "usna")$code, haver.metadata(series1, 
#   "usna")$descriptor)  # use this for reference
#####

data2 <-
  pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ"), 
                  "usecon",
            start.date = START)


# pull annual BEA NIPAs data, for regressions for state and
# local tax revenues

### sERIES 3 ####
series3 = c("GDP", "C", "CH", "GDPH", "JC", "JGDP", "JGF", "JGS", 
            "PTGH", "PTGSH", "PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", 
            "YPTX", "YTPI", "YCTLG", "G", "GRCSI", "GDPH", "DC", "PTGFH", 
            "PTGSH", "GF", "GS", "GFH", "GSH", "\tGFRPT", "GFRPRI", "GFRCP", 
            "GFRS", "GFRPT", "\tGFRPRI", "\tGFRCP", "\tGFRS", "\tGFTFP", 
            "\tGFEG", "\tGSRPT", "\tGSRPRI", "\tGSRCP", "\tGSRS", "\tGSTFP", 
            "\tGSET", "YP", "GFSUB", "GSSUB")

data3 = pull_data(series3, "usna", start = as.Date("1970-01-01"), 
                  frequency = "annual") %>%
  as_tibble()

### sERIES 4 ####
series4 = c("USPHPI", "CASUSXAM", "GDPPOT", "GDPPOTH")
data4 <-
  pull_data(series4,
            "usecon",
            start = START, 
            frequency = "annual") %>%
  as_tibble()


# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts_quarterly = data1,
       economic_quarterly = data2,
       national_accounts_annual = data3,
       economic_annual = data4)

# Write haver data to raw folder ------------------------------------------


# Step 1
# Define a function for exporting csv with the desired file names and into the right path
output_csv <- function(data, names){ 
  folder_path <- "data/raw/haver/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}

# Step 2
list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  
  # Step 3
  purrr::pmap(output_csv) 


write_xlsx(data1, "data/raw/data1.xlsx")
write_xlsx(data2, "data/raw/data2.xlsx")
write_xlsx(data3, "data/raw/data3.xlsx")
write_xlsx(data4, "data/raw/data4.xlsx")

