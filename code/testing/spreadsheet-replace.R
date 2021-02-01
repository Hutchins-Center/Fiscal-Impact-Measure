# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('readxl')
library('writexl')
library('tsibble')

source('src/functions.R')

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "12-30-2018"

# Quarterly -------------------------------------------------------------------------------------------------------
#haver.path("//ESDATA01/DLX/DATA/")
# BEA NIPAs
names_usna <- read_excel("data/auxilliary/spreadsheet_names.xlsx")
# data1 <-
#   pull_data(names_usna$code,
#             "usna",
#             start.date = START) %>%
#   as_tibble()



df <-
  read_xlsx('data/add-ons/LSFIM_KY_v7.xlsx',
            sheet = 'Haver NA') %>%
  pivot_longer(-variable)  %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(
    date = name,
    subsidies = gsubx,
    ppp = gfsubp,
    nonprofit_ppp = gftfpp,
    nonprofit_provider_relief = gftfpv,
    aviation = gfsubg,
    employee_retention = gfsube,
    sick_leave = gfsubk,
    rebate_checks = gftfpe,
    ui_bea = yptux,
    total_grants = gfegx,
    legislation_grants = gfegl,
    medicaid_total = yptmdx,
    medicaid_grants = gfeghdx,
    medicare = yptmrx,
    social_benefits_nipa =  gftfpx,
    state_social_benefits = gsetfpx
  ) %>%
  mutate(
    date = yearquarter(date),
    across(where(is.numeric), ~ . / 1000),
    other_subsidies  =  aviation + employee_retention + sick_leave,
    legislation_subsidies = ppp + other_subsidies, 
    nonprofit_subsidies = nonprofit_ppp +  nonprofit_provider_relief,
    non_medicaid_grants = total_grants - medicaid_grants,
    non_medicaid_or_legislation_grants = non_medicaid_grants - legislation_grants,
    state_local_grants =  c(rep(0, 6), 35, 45, 60),
    education_grants = c(rep(0, 6), 25,  25, 35),
    hospital_grants = c(rep(0, 6), 27, 27, 12),
    other_grants = state_local_grants + education_grants + hospital_grants,
    federal_cgrants = non_medicaid_or_legislation_grants + other_grants,
    federal_health  = medicaid_grants + medicare,
    state_health = medicaid_total  - medicaid_grants,
    state_ui = 0 #PLACEHOLDER
  ) %>%
  mutate(social_benefits_remainder = 
            social_benefits_nipa - medicare - nonprofit_subsidies -  ui_bea -  30 - rebate_checks - 5,
         fim_state_social_benefits = state_social_benefits - medicaid_grants - state_ui,
         federal_social_benefits = 5 + nonprofit_subsidies + ui_bea + 30 + rebate_checks + social_benefits_remainder,
         x = social_benefits_nipa - medicare + 5 + 30,
         y = social_benefits_nipa - medicare - nonprofit_subsidies -  ui_bea -  30 - rebate_checks - 5 +
           nonprofit_subsidies + ui_bea
         )

# Federal health
# State health
# Subsidies
# Grants
# Basically, to get the FIM social benefits we subtract medicare from NIPA social benefits
# and then we add the  difference between their ui and ours 
# 
# fim ex everything = social benefits - 
