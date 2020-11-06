
# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('Haver')
library('readxl')
library('writexl')

source('src/functions.R')

# 0.1 Pull Raw Data---------------------------------------------------------------

# We translate annual levels into quarterly values by imputing them with the 4-quarter moving average
# Quarterly values are in seasonally-adjusted, annual rates (SAAR), billions of dollars. 
# Annual values are in annual rates, billions of dollars. 

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------

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


# Annual ----------------------------------------------------------------------------------------------------------

# BEA NIPAs data, for regressions for state and local tax revenues

data3 <- 
  haver.data(names_usna$code, 'usna', start = as.Date('1970-01-01'), frequency = 'annual',
             eop.dates = TRUE) %>%
  data.frame(date = rownames(.), .) %>%
  as_tibble()

### sERIES 4 ####
series4 = c("USPHPI", "CASUSXAM", "GDPPOT", "GDPPOTH")
data4 <-
  haver.data(series4,
            "usecon",
            start = as.Date('1970-01-1'), 
            frequency = "annual",
            eop.dates = TRUE) %>%
  data.frame(date = rownames(.), .) %>%
  as_tibble()


# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts_quarterly = data1,
       economic_quarterly = data2,
       national_accounts_annual = data3,
       economic_annual = data4)

# Write haver data to raw folder ------------------------------------------


## Exporting csv with the desired file names and into the right path
output_csv <- function(data, names){ 
  folder_path <- "data/raw/haver/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}


list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  purrr::pmap(output_csv) 