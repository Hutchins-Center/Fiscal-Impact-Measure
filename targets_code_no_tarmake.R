library(targets)
library(tarchetypes)
source('R/packages.R')
source('R/functions.R')
library('conflicted')
conflicted::conflict_prefer('filter', 'dplyr')
conflict_prefer("month", "lubridate")
conflict_prefer('year', 'lubridate')
conflict_prefer('lag', 'dplyr')

current_month <- glue::glue(lubridate::month(lubridate::today()), '-', lubridate::year(lubridate::today()))
components <- get_components_names()
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")
growth_rates <- function(df){
  df %>%
    mutate(
      across(
        .cols = where(is.numeric) & !ends_with('_growth') &!ends_with('_g'),
        .fns = ~ q_g(.),
        .names = "{.col}_g"
      ) 
    )
}
mpc_unemployment_insurance <- 
  function(df){
  weights <- c(rep(0.05, 2), rep(0.1, 2), rep(0.35, 2))
  df %>% 
    mutate(
      across(all_levels('unemployment_insurance_minus_neutral'),
             ~ 0.9 * roll::roll_sum(.x, weights = weights, width = length(weights), online = FALSE))
    ) 
}
taxes_transfers_minus_neutral <- function(df){
  taxes = all_levels('corporate_taxes', 'non_corporate_taxes')
  transfers = all_levels('social_benefits', 'health_outlays', 'subsidies', 'unemployment_insurance', 'rebate_checks')
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = any_of(all_levels(taxes, transfers)),
                    .fns = ~ . - dplyr::lag(.) * (1 + gdppoth + pi_pce),
                    .names = '{.col}_minus_neutral')
    )
}
mpc_unemployment_insurance <- mpc_ui
# End this file with a list of target objects.

    cbo_projections_raw = load_cbo_projections()
    fmap = read_xlsx(here('data/raw/nhe_fmap.xlsx'))
    historical = load_national_accounts() %>% mutate(date = as_date(date))
    last_hist_date = get_last_hist_date(historical)
    last_proj_date = last_hist_date + lubridate::years(2)
    cbo_projections = load_cbo_projections() %>%
      cola_adjustment() %>% 
      smooth_budget_series() %>%
      federal_transfers_growth_rates() %>%
      health_outlays_growth_rates() %>%
      alternative_tax_scenario() %>%
      implicit_price_deflators() %>%
      state_taxes() %>%
      growth_rates()
    unemployment_insurance_override = load_unemployment_insurance_override()
    components = get_components_names()
    combined_data =
      full_join(
        historical,
        cbo_projections %>%
          dplyr::select(date, tidyselect::contains('_g')),
        by = 'date'
      ) %>%
      left_join(unemployment_insurance_override) %>%
      mutate(across(
        .cols = ends_with('override'),
        .fns = ~ if_else(is.na(.x), 0, .x)
      )) %>%
      millions_to_billions()
    #comma here? if breaks

    projections =
      combined_data %>%
      mutate(date = as_date(date)) %>% 
      unemployment_insurance_reallocation() %>%
      fmap_share_old() %>%
      mutate(historical = if_else(date > last_hist_date, 0, 1)) %>%
      mutate(forecast_period = if_else(date <= last_hist_date, 0, 1)) %>%
      group_by(forecast_period) %>%
      create_override(
        var = gs_g,
        start = '2020-12-31',
        end = '2022-03-31',
        values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
      ) %>% 
      growth_rates() %>% 
      create_projections() %>% 
      medicaid_reallocation()
    
    fim =
      fim_create(projections) %>%
      mutate(id =  if_else(date <= last_hist_date, 'historical', 'projection')) %>%
      add_factors() %>%
      override_projections() %>%
      fill_overrides() %>% 
      mutate(date2 = yearquarter(date)) %>% 
      as_tsibble(index = date2) %>% 
      full_join(read_xlsx('data/pandemic-legislation/arp_summary.xlsx') %>% 
                  mutate(date2 = yearquarter(date)) %>% filter(date2 > yearquarter('2021 Q1')) %>% mutate(date = as_date(date2) - 1), by = 'date') %>% 
      as_tibble() %>% 
      mutate(federal_cgrants = coalesce(federal_cgrants_override, federal_cgrants)) %>%
      contributions_purchases_grants() %>%
      total_purchases() %>% 
      rename(non_corporate_taxes = noncorp_taxes,
             federal_non_corporate_taxes = federal_noncorp_taxes,
             state_non_corporate_taxes = state_noncorp_taxes) %>% 
      taxes_transfers_minus_neutral() %>%
      mutate(across(where(is.numeric),
                    ~ coalesce(.x, 0))) %>% 
      mutate(across(.cols = all_of(c('rebate_checks_arp', 'other_direct_aid', 
                                     'health_grants_arp', 'non_health_grants',
                                     'other_vulnerable', 'federal_ui_arp', 'state_ui_arp', 'aid_to_small_businesses')),
                    .fns = ~ .x - dplyr::lag(.x) * (1 + gdppoth + pi_pce),
                    .names = '{.col}_minus_neutral'))  %>% 
      mpc_subsidies() %>%
      mpc_health_outlays() %>% 
      mpc_social_benefits() %>% 
      mpc_unemployment_insurance() %>% 
      mutate(across(all_levels('rebate_checks'),
                    ~ mpc_rebate_checks(.x))) %>% 
      mpc_non_corporate_taxes() %>% 
      mpc_corporate_taxes() %>% 
      rename_with(~ gsub('minus_neutral', 'post_mpc', .x)) %>% 
      rename_with(~ gsub('arp_post_mpc', 'arp_minus_neutral', .x)) %>% 
      rename(other_vulnerable_minus_neutral = other_vulnerable_post_mpc,
             other_direct_aid_minus_neutral =  other_direct_aid_post_mpc,
             aid_to_small_business_minus_neutral = aid_to_small_businesses_post_mpc)  %>% 
      taxes_contributions() %>% 
      sum_taxes_contributions() %>% 
      mpc_arp_non_health_grants() %>% 
      mutate(non_health_grants_cont = 400 * (non_health_grants_post_mpc  - lag(non_health_grants_post_mpc) * (1 + gdppoth + federal_cgrants_pi)) / lag(gdp)) %>% 
      mutate(federal_grants_cont = federal_grants_cont + non_health_grants_cont,
             federal_nom = federal_nom + non_health_grants,
             federal_nom_cont = federal_nom_cont + non_health_grants_cont,
             federal_cont_no_arp = federal_cont, 
             federal_cont = federal_cont + non_health_grants_cont,
             purchases_cont = purchases_cont + non_health_grants_cont) %>% 
      mutate(aid_to_small_businesses_minus_neutral_post_mpc = mpc_small_businesses_arp(aid_to_small_business_minus_neutral)) %>% 
      mutate(across(.cols = all_of(c('federal_ui_arp', 'state_ui_arp', 'other_vulnerable') %>% paste0('_minus_neutral')),
                    .fns = ~ mpc_vulnerable_arp(.x),
                    .names = '{.col}_post_mpc'),
             across(.cols = all_of(c('rebate_checks_arp','other_direct_aid') %>% paste0('_minus_neutral')),
                    .fns = ~ mpc_direct_aid_arp(.),
                    .names = '{.col}_post_mpc'),
             health_grants_arp_minus_neutral_post_mpc =  0.9*roll::roll_mean(health_grants_arp_minus_neutral, width = 4, online = FALSE)) %>% 
      
      # mutate(arp = rebate_checks_arp + other_direct_aid + health_grants_arp + non_health_grants + other_vulnerable + federal_ui_arp + state_ui_arp + aid_to_small_businesses,
      #        arp_post_mpc_minus_neutral = rebate_checks_arp_post_mpc_minus_neutral + other_direct_aid_post_mpc_minus_neutral + federal_ui_arp_post_mpc_minus_neutral + state_ui_arp_post_mpc_minus_neutral + other_vulnerable_post_mpc_minus_neutral + non_health_grants_post_mpc_minus_neutral + aid_to_small_businesses_post_mpc_minus_neutral ) %>% 
      transfers_contributions() %>% 
      sum_transfers_contributions() %>% 
      rename(federal_taxes_cont = federal_taxes_contribution,
             state_taxes_cont = state_taxes_contribution) %>% 
      mutate(
        federal_taxes_transfers_cont = federal_taxes_cont + federal_transfers_cont,
        state_taxes_transfers_cont = state_taxes_cont + state_transfers_cont,
        taxes_transfers_cont = federal_taxes_transfers_cont + state_taxes_transfers_cont
      ) 
      add_social_benefit_components() 
      mutate(
        federal_social_benefits = federal_social_benefits + other_direct_aid + other_vulnerable + rebate_checks_arp + federal_ui_arp,
        state_social_benefits = state_social_benefits + state_ui_arp,
        
        federal_subsidies = federal_subsidies + aid_to_small_businesses,
        subsidies = federal_subsidies + state_subsidies,
        
        federal_health_outlays = federal_health_outlays + health_grants_arp,
        health_outlays = federal_health_outlays + state_health_outlays,
        
        
        
        
        federal_transfers = federal_social_benefits + federal_subsidies + federal_health_outlays ,
        state_transfers = state_social_benefits + state_health_outlays + state_subsidies,
        transfers = federal_transfers + state_transfers) %>% 
    
        get_fiscal_impact() %>%
      mutate(fiscal_impact = federal_cont + state_local_cont + taxes_cont + state_transfers_cont +  federal_transfers_cont_no_arp + federal_ui_arp_cont + rebate_checks_arp_cont + aid_to_small_businesses_cont + health_grants_arp_cont + other_direct_aid_cont + other_vulnerable_cont,
             arp_cont =  health_grants_arp_cont + non_health_grants_cont +
               federal_ui_arp_cont + rebate_checks_arp_cont + other_direct_aid_cont + other_vulnerable_cont + aid_to_small_businesses_cont) %>% 
      
      arrange(date, recession, fiscal_impact, fiscal_impact_moving_average,
              federal_cont, state_local_cont,
            taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont)
    
    
fim %>% filter(date > "2020-06-30") %>% select(date, fiscal_impact)  
    
    
    
    