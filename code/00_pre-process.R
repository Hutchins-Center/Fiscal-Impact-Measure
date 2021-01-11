# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('Haver')
library('readxl')
library('writexl')

source('src/functions.R')

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------
haver.path("//ESDATA01/DLX/DATA/")
# BEA NIPAs 
names_usna <- read_excel("data/auxilliary/haver_names.xlsx")
data1 <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble()

# Economic Statistics

data2 <-
  pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ"), 
            "usecon",
            start.date = START)

data3 <-
  pull_data(c('LASGOVA', 'LALGOVA'),
            'usecon',
            start.date = START)



# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts = data1,
       economic_statistics = data2,
       state_local_employment = data3)

# Write haver data to raw folder ------------------------------------------


## Exporting csv with the desired file names and into the right path
output_xlsx <- function(data, names){ 
  folder_path <- "data/raw/haver/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}


list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  purrr::pmap(output_xlsx) 