source('src/packages.R')
devtools::install_github("malcalakovalski/fim") 
library(fim)

load_economic_projections <- function(){
  read_xlsx(here('data/raw/cbo/cbo_econ_proj_quarterly.xlsx')) %>%
    mutate(date = as_date(date))
}
load_budget_projections <- function(){
  read_xlsx(here('data/raw/cbo/cbo_budget_nipas_proj_annual.xlsx')) %>%
    as_tsibble(index = fy)
}
load_cbo_projections <- function(){
  budget <- load_budget_projections()
  economic <- load_economic_projections()
  
  budget %>%
    annual_to_quarter(fy) %>%
    left_join(economic)
}
load_unemployment_insurance_override <- function(){
  read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
             sheet = "FIM Add Factors") %>%
    mutate(date = as_date(date)) %>%
    select(date, contains('unemployment_insurance'))
}
get_haver_data <- function(){
  # Load U.S. national accounts and economic statistics data into the Global Environment
  haver_data_path <-
    here('data/raw/haver/')
  haver_data_names <- 
    haver_data_path  %>%
    list.files() %>%
    .[str_detect(., ".xlsx")]
  
  # Load raw Haver data into global environment
  haver_data_names %>%  
    purrr::map(function(file_name){ # iterate through each file name
      assign(x = str_remove(file_name, ".xlsx"), 
             value = read_xlsx(paste0(haver_data_path,"/", file_name), na = 'NA') %>%
               mutate(date = as.Date(date)),
             envir = .GlobalEnv)
    }) 
  # Merge quarterly and annual data 
  # change hist and aa to haver quarterly and annual
  left_join(national_accounts,
            economic_statistics,
            by = "date") 
}
econ <- load_economic_projections()
budg <- load_budget_projections()
fmap <- read_xlsx(here('data/raw/nhe_fmap.xlsx'))
hist <- get_haver_data()

last_hist_date <-
  hist %>%
  pull(date) %>%
  max()

last_proj_date <- last_hist_date + years(2)

cbo_projections <- load_cbo_projections()

cbo_projections <-
  cbo_projections %>%
    cola_adjustment() %>%
    smooth_budget_series() %>%
    federal_transfers_growth_rates() %>%
    health_outlays_growth_rates() %>%
    alternative_tax_scenario() %>%
    implicit_price_deflators() %>%
    state_taxes() %>%
    growth_rates()
  
unemployment_insurance_override <- load_unemployment_insurance_override()
projections <-
  full_join(hist,
            cbo_projections %>%
              select(date, contains('_g')),
            by = 'date') %>%
  left_join(unemployment_insurance_override) %>%
  mutate(
    across(
      .cols = ends_with('override'),
      .fns = ~ if_else(is.na(.x), 0, .x)
    )
  )

projections <-
  projections %>%
    millions_to_billions() %>%
    unemployment_insurance_reallocation() %>%
    fmap_share_old() %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth() 
components <- get_components_names()
for(f in get_forecast_period()){
  projections[f,components] = projections[f-1, components]  * (1 + projections[f, paste0(components, "_g")])
}

projections <- 
  projections %>%
    sum_projections(gtfp, gftfp, gstfp) %>%
    sum_projections(yptx, gfrpt, gsrpt) %>%
    sum_projections(ytpi, gfrpri, gsrpri) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(yctlg, gfrcp, gsrcp) %>%
    sum_projections(gsub, gfsub, gssub) %>%
    medicaid_reallocation()

fim <- fim_create(projections)

fim <-
  fim %>%
      add_factors() %>%
      override_projections() %>%
      fill_overrides() %>%
      contributions_purchases_grants() %>%
      total_purchases() %>%
      remove_social_benefit_components() %>%
      taxes_transfers_minus_neutral() %>%
      calculate_mpc('subsidies') %>%
      calculate_mpc('health_outlays') %>%
      calculate_mpc('social_benefits') %>%
      calculate_mpc('unemployment_insurance') %>%
      calculate_mpc('rebate_checks') %>%
      calculate_mpc('noncorp_taxes') %>%
      calculate_mpc('corporate_taxes') %>%
      taxes_contributions() %>%
      sum_taxes_contributions() %>%
      transfers_contributions() %>%
      sum_transfers_contributions() %>%
      sum_taxes_transfers() %>%
      add_social_benefit_components() %>%
      get_fiscal_impact() %>%
      arrange(date, recession, fiscal_impact, fiscal_impact_moving_average, 
              federal_cont, state_local_cont, 
              taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont)

fim_interactive <-
  fim %>%
    prepare_interactive()
  
prepare_results()

start <- as_date("2000-01-01")
end <- as_date("2022-12-31")

total_pink <- rgb(231, 97, 159, maxColorValue = 255)
state_local_purple = rgb(174, 104, 169,  maxColorValue = 255)
federal_blue = rgb(33, 152, 199,  maxColorValue = 255)
taxes_transfers_green = rgb(27, 149, 83,  maxColorValue = 255)

contributions <- load_contributions()
max_y <- contributions %>% max_y()

recession_shade <- 
  contributions %>%
  get_recession_shade()

total <-
  contributions %>%
  select(date, fiscal_impact, fiscal_impact_moving_average) %>%
  pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
  fim_plot(title = 'Total') +
  scale_fill_manual(labels = " Quarterly fiscal impact",
                    values = total_pink) 
components <- 
  contributions %>%
  select(date,  state_local_cont, federal_cont, taxes_transfers_cont, fiscal_impact_moving_average) %>%
  pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
  fim_plot(title = 'Total') +
  scale_fill_manual(
    labels = c(" State & Local Purchases", " Federal Purchases", " Taxes, Transfers, & Subsidies"),
    values =  c(state_local_purple, federal_blue, taxes_transfers_green)
  )
