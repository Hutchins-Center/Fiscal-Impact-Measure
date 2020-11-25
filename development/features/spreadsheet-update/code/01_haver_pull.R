# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('Haver')
library('readxl')
library('writexl')

source('src/functions.R')

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "12-30-2018"

# Quarterly -------------------------------------------------------------------------------------------------------
#haver.path("//ESDATA01/DLX/DATA/")
# BEA NIPAs 
names_usna <- read_excel("data/auxilliary/spreadsheet_names.xlsx")
data1 <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble()

data1 <- data1 %>% mutate(
  gsrsp = (csipslx + csislx) / 1000,
  gftfpe = gftfpe * 1000,
  gftfpp = gftfpp * 1000,
  gftfpv = gftfpv * 1000,
  gfsubp = gfsubp * 1000,
  gfsubg = gfsubg * 1000,
  gfsube = gfsube * 1000,
  gfsubk = gfsubk * 1000,
  gsubl = (gfsubg + gfsube + gfsubk),
  gfegl = (gfegc + gfege + gfegv ) * 1000
) %>% select(-c(gfegc, gfege, gfegv))


#transpose into spreadsheet layout
data1_t <- as.data.frame(t(data1))

colnames(data1_t) <- as.character(unlist(data1_t[1,]))

data1_t <- data1_t[-1,]

data1_t <- tibble::rownames_to_column(data1_t, "variable")



# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts = data1_t)

# Write haver data to raw folder ------------------------------------------


## Exporting csv with the desired file names and into the right path
output_xlsx <- function(data, names){ 
  folder_path <- "development/features/spreadsheet-update/output/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}


list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  purrr::pmap(output_xlsx) 