# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('readxl')
library('writexl')
library('tsibble')
library('janitor')

source('src/functions.R')
library('lubridate')

annual_to_quarter <- function(df){
  df %>%
    slice(rep(1:n(), each=4)) %>%
    rename(fy = date) 
}
# 0.1 Pull Raw Data---------------------------------------------------------------

START <- as_date("2018-12-31")
nonprofit_hospitals <- 0.5
government_hospitals <- 0.2
forprofit_hospitals <- 0.3



cbo_legislation <-
  read_xlsx('data/pandemic-legislation/pandemic-legislation.xlsx',
            sheet = 'annual') %>%
    select(-1) %>%
    pivot_longer(-date) %>%
    pivot_wider(names_from = date, values_from = value) %>%
    rename(year = name) %>%
    clean_names() %>%
    mutate(
      across(everything(), ~ replace_na(., 0)),
      across(where(is.numeric), ~ . * 4)
    ) %>%
  fim::annual_to_quarter(year)

provider_relief_fund_score <- 
  cbo_legislation %>%
  mutate(provider_relief_fund = nonprofit_hospitals * provider_relief_fund ) %>%
  pull(provider_relief_fund)
# Quarterly -------------------------------------------------------------------------------------------------------
#haver.path("//ESDATA01/DLX/DATA/")
# BEA NIPAs
names_usna <- read_excel("data/auxilliary/spreadsheet_names.xlsx")
usna <- read_xlsx('data/raw/haver/national_accounts.xlsx') %>%
  filter(date >= as_date('2012-12-31')) %>%
  rename(
    subsidies = gsub,
    ppp = gfsubp,
    nonprofit_ppp = gftfpp,
    nonprofit_provider_relief = gftfpv,
    aviation = gfsubg,
    employee_retention = gfsube,
    sick_leave = gfsubk,
    rebate_checks = gftfpe,
    ui_bea = yptu,
    total_grants = gfeg,
    covid_relief_fund = gfegc,
    education_stabilization_fund = gfege,
    provider_relief_fund = gfegv,
    medicaid_total = yptmd,
    medicaid_grants = gfeghdx,
    medicare = yptmr,
    social_benefits_nipa =  gftfp,
    state_social_benefits = gstfp
  ) %>%
  mutate(
    date = yearquarter(date),
    other_subsidies  =  aviation + employee_retention + sick_leave,
    legislation_subsidies = ppp + other_subsidies, 
    nonprofit_subsidies = nonprofit_ppp +  nonprofit_provider_relief,
    legislation_grants = covid_relief_fund + education_stabilization_fund + provider_relief_fund,
    non_medicaid_grants = total_grants - medicaid_grants,
    non_medicaid_or_legislation_grants = non_medicaid_grants - legislation_grants,
    other_grants = state_local_grants + education_grants + hospital_grants,
    federal_cgrants = non_medicaid_or_legislation_grants + other_grants,
    federal_health  = medicaid_grants + medicare,
    state_health = medicaid_total  - medicaid_grants,
    state_ui = yptu - yptolm)
mpc_covid_relief_fund <- 
  function(x){
    mpc <- 1
    weights <- c(0.0583, 0.075, rep(0.1, 2), rep(0.08333, 8))
    mpc * roll::roll_sum(x, width = length(weights),
                         weights = rev(weights), online = FALSE)
  }

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
