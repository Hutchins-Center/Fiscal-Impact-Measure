source('R/packages.R')
library('lubridate')
library('janitor')
library('tsibble')
library('dplyr')
legislation_monthly <-
  read_xlsx('data/pandemic-legislation/pandemic-legislation.xlsx',
            sheet = 'monthly') %>%
    pivot_longer(where(is.numeric)) %>%
    pivot_wider(names_from = date, values_from = value) %>% 
    rename(date = name) %>%
    mutate(date = as_date(as.numeric(date), origin = '1900-01-01'))  %>%
    clean_names() %>%
    mutate(unemployment_insurance_total = unemployment_insurance + wages_lost_assistance_program)
legislation_quarterly <- 
  read_xlsx('data/pandemic-legislation/pandemic-legislation.xlsx',
            sheet = 'quarterly') %>%
    pivot_longer(where(is.numeric)) %>%
    pivot_wider(names_from = date, values_from = value) %>% 
    rename(date = name) %>%
      mutate(date = yearquarter(date)) %>%
    clean_names() 
fill_quarter <- function(df){
  length <- df %>%
    count() %>% pull()
  missing_months <- 3 - length %% 3
  df %>%
    mutate(date  = yearmonth(date)) %>%
    mutate(quarter = yearquarter(date))  %>% 
    tsibble::as_tsibble(index = date) %>%
    tsibble::append_row(n = missing_months) %>%
    fill(quarter) %>%
    select(date, quarter, tidyselect::everything())
}

read_xlsx('data/pandemic-legislation/pandemic-legislation.xlsx',
          sheet = 'annual') %>%
  select(-1) %>%
  pivot_longer(where(is.numeric)) %>%
  pivot_wider(names_from = date, values_from = value) %>%
  rename(year = name)  %>%
  annual_to_quarter(year)

  

    

