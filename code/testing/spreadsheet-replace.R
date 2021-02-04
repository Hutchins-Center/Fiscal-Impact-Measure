# NOTES
# We had the wrong UI numbers in the spreadsheet.
# We never updated the wages lost assistance spending, which was higher than we expected
# in Q3. Unfortunately, the previous number was hardcoded so not sure where it came from.
# It said Total Q3 was 881,354

# UI Q3 was also
# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('readxl')
library('writexl')
library('tsibble')
library('janitor')

source('src/functions.R')
library('lubridate')
#source('_drake.R')
annual_to_quarter <- function(df){
  df %>%
    slice(rep(1:n(), each=4)) %>%
    rename(fy = date) 
}
rename_haver_codes <- function(df){
  df %>%
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
      federal_social_benefits_nipa =  gftfp,
      state_social_benefits = gstfp,
      wages_lost_assistance = yptolm,
      ui_bea = yptu,
      peuc = yptue,
      pua = yptup,
      puc = yptuc
    )
}
# 0.1 Pull Raw Data---------------------------------------------------------------
loadd(projections)
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
  filter(date >= as_date('2015-12-31')) %>%
  rename_haver_codes() %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)), 
    date = yearquarter(date),
    # SUBSIDIES
    other_subsidies  =  aviation + employee_retention + sick_leave,
    legislation_subsidies = ppp + other_subsidies, 
    nonprofit_subsidies = nonprofit_ppp +  nonprofit_provider_relief,
    # GRANTS
    legislation_grants = covid_relief_fund + education_stabilization_fund + provider_relief_fund,
    medicaid_grants = medicaid_grants / 1000, # MILLIONS TO BILLIONS
    non_medicaid_grants = total_grants - medicaid_grants,
    non_medicaid_or_legislation_grants = non_medicaid_grants - legislation_grants,
    other_grants = legislation_grants, # PLACEHOLDER
    federal_cgrants = non_medicaid_or_legislation_grants + other_grants,
    # HEALTH
    federal_health  = medicaid_grants + medicare,
    state_health = medicaid_total  - medicaid_grants,
    # SOCIAL BENEFITS
    
    # UNEMPLOYMENT INSURANCE
    ui = ui_bea + wages_lost_assistance,
    ui_cbo_assumed_other = 
      case_when(
        date < yearquarter('2020 Q2') ~ 0,
        date >=  yearquarter('2020 Q2') & date <=  yearquarter('2020 Q3') ~ 12,
        date >=  yearquarter('2020 Q4') & date <= yearquarter('2021 Q1') ~ 8,
        date >= yearquarter('2021 Q2') ~ 4
      ),
    federal_ui = 2 * peuc + pua + puc + wages_lost_assistance + ui_cbo_assumed_other,
    state_ui = ui - federal_ui,
    # FEDERAL SOCIAL BENEFITS
    federal_social_benefits = federal_social_benefits_nipa - medicare - state_ui,
    state_social_benefits = state_social_benefits + state_ui - medicaid_grants 
    ) 


# Forecast ------------------------------------------------------------------------------------


# Consumption Grants --------------------------------------------------------------------------

totals <- function(df, var){
  df %>% 
    as_tibble() %>%
    select(date, var) %>%
    pivot_wider(-date, names_from = date, values_from = var) %>%
    summarise(sum(c_across(everything()))) %>%
    pull()
}

cbo_education <- 4 * 30 # Annualize

education_total_disbursed <-
  usna %>%
  totals(var = 'education_stabilization_fund')

education_total_remaining <-
  cbo_education - education_total_disbursed
mpc_education_stabilization_fund <- function(x, n){
  mpc <- 1
  weights <- c(rep(1 / n, n))
  x <- x * weights
  return(x)
}


x <- mpc_education_stabilization_fund(education_total_remaining, n = 4)

df <-
  tibble(
    date = seq(yearquarter('2021 Q1'), length.out = 4, by = 1),
    x = x
  )
usna %<>%
  as_tsibble(index = date) %>%
  tsibble::append_row(n = 12) %>%
  left_join(projections %>% mutate(date = yearquarter(date)) %>%
              select(date, gfeg, gfeghdx)) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

usna %>%
  mutate(education_stabilization_fund = 
           case_when(
             date < yearquarter('2021 Q1') ~ education_stabilization_fund,
             date >= yearquarter('2021 Q1') & date <= yearquarter('2022 Q2') ~ education_total_remaining 
           ))

# State Health --------------------------------------------------------------------------------


cbo_state_health <-
  tibble(
    fy = as.numeric(c(2020:2022)),
    cbo_state_health = c(657,	726,	727),
    growth_rate = ( cbo_state_health / lag(cbo_state_health))
  )

cbo_state_health_growth <-   
  cbo_state_health %>%
  filter(fy == 2021) %>%
  pull(growth_rate)

forecasts <- 
  tibble(
    date = seq(yearquarter("2021 Q1"), length.out = 12, by = 1), # by 1 quarter
    growth = case_when(
      date == yearquarter('2021 Q1') ~ cbo_state_health_growth^0.18,
      date == yearquarter('2021 Q2') ~ cbo_state_health_growth^0.1,
      date >= yearquarter('2021 Q3') ~ NaN
      )
  ) %>%
 mutate(growth = zoo::na.locf(growth))


# Total Medicaid ------------------------------------------------------------------------------


usna %>% 
  full_join(forecasts, by = 'date') %>%
  mutate(
    medicaid_total = if_else(
      date < yearquarter('2021 Q1'),
      medicaid_total,
      lag(medicaid_total) * growth
    ),
    medicaid_total = if_else(
      date == yearquarter('2021 Q2'),
      lag(medicaid_total) * growth,
      medicaid_total
    ),
    medicaid_total = zoo::na.locf(medicaid_total),
    medicaid_grants = case_when(date < yearquarter('2021 Q1') ~ medicaid_grants,
                                date >= yearquarter('2021 Q1') & date <= yearquarter('2022 Q1') ~ medicaid_total * 0.74,
                                date > yearquarter('2021 Q1') ~ medicaid_total * 0.68),
    state_health_outlays = medicaid_total - medicaid_grants,
    federal_health_outlays = medicaid_grants + medicare
  ) 
  



# Misc ----------------------------------------------------------------------------------------


fill_in <- function(prev, new, growth = 0.03) {
  if_else(!is.na(new), new, prev * (1 + growth))
}

# fim_state_social_benefits = (nipa - medicare - ui - rebate_checks - nonprofit_subsidies) + rebate_checks + nonprofit_subsidies + federal_ui
#                           = nipa - medicare + (federal_ui - ui) 
#                           = nipa - medicare - state_ui
mpc_covid_relief_fund <- 
  function(x){
    mpc <- 1
    weights <- c(0.0583, 0.075, rep(0.1, 2), rep(0.08333, 8))
    mpc * roll::roll_sum(x, width = length(weights),
                         weights = rev(weights), online = FALSE)
  }


# Federal health
# State health
# Subsidies
# Grants
# Basically, to get the FIM social benefits we subtract medicare from NIPA social benefits
# and then we add the  difference between their ui and ours 
# 
# fim ex everything = social benefits - 
