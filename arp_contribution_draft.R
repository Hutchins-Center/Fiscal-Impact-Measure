tar_load(projections) 
tar_load(last_hist_date)
tar_load(last_proj_date)


fim_create(projections) %>%
  mutate(id =  if_else(date <= last_hist_date, 'historical', 'projection')) %>%
  add_factors() %>%
  override_projections() %>%
  fill_overrides() %>% 
  full_join(read_xlsx('data/pandemic-legislation/arp_summary.xlsx') %>% 
              mutate(date = yearquarter(date))) %>% 
  select(rebate_checks_arp) 

tar_load(projections)
tar_load(last_hist_date)
tar_load(last_proj_date)
df <- 
  fim_create(projections) %>%
  mutate(id =  if_else(date <= last_hist_date, 'historical', 'projection')) %>%
  add_factors() %>%
  override_projections() %>%
  fill_overrides() %>% 
  mutate(date2 = yearquarter(date)) %>% 
  as_tsibble(index = date2) %>% 
  full_join(read_xlsx('data/pandemic-legislation/arp_summary.xlsx') %>% 
              mutate(date2 = yearquarter(date)) , by = 'date2') %>% 
  rename(date = date.x) %>% 
  as_tibble() %>% 
  mutate(federal_cgrants = coalesce(federal_cgrants_override, federal_cgrants)) %>%
  contributions_purchases_grants() %>%
  total_purchases() %>%
  remove_social_benefit_components() %>%
  taxes_transfers_minus_neutral() %>%
  mutate(across(where(is.numeric),
                ~ coalesce(.x, 0))) %>% 
  mutate(across(.cols = all_of(c('rebate_checks_arp', 'other_direct_aid', 
                                 'health_grants_arp', 'non_health_grants',
                                 'other_vulnerable', 'federal_ui_arp', 'state_ui_arp', 'aid_to_small_businesses')),
                .fns = ~ .x - dplyr::lag(.x) * (1 + gdppoth + pi_pce),
         .names = '{.col}_minus_neutral')) %>% 
  calculate_mpc('subsidies') %>%
  calculate_mpc('health_outlays') %>%
  calculate_mpc('social_benefits') %>%
  calculate_mpc('unemployment_insurance') %>%
  calculate_mpc('rebate_checks') %>%
  calculate_mpc('noncorp_taxes') %>%
  calculate_mpc('corporate_taxes') %>% 
  taxes_contributions() %>%
  sum_taxes_contributions() %>% 

  mpc_arp_non_health_grants() %>% 
  mutate(non_health_grants_cont = 400 * (non_health_grants_post_mpc  - lag(non_health_grants_post_mpc) * (1 + gdppoth + federal_cgrants_pi)) / lag(gdp)) %>% 
  mutate(federal_grants_cont = federal_grants_cont + non_health_grants_cont,
         federal_cont = federal_cont + non_health_grants_cont,
         purchases_cont = purchases_cont + non_health_grants_cont) %>% 
  mutate(across(.cols = all_of(c('federal_ui_arp', 'state_ui_arp', 'other_vulnerable') %>% paste0('_minus_neutral')),
                .fns = ~ mpc_vulnerable_arp(.x),
                .names = '{.col}_post_mpc'),
         across(.cols = all_of(c('rebate_checks_arp', 'other_direct_aid') %>% paste0('_minus_neutral')),
                .fns = ~ mpc_direct_aid_arp(.),
                .names = '{.col}_post_mpc'),
         health_grants_arp_minus_neutral_post_mpc = mpc_health_outlays(health_grants_arp_minus_neutral),
         aid_to_small_businesses_minus_neutral_post_mpc = mpc_small_businesses_arp((aid_to_small_businesses_minus_neutral))
  ) %>% 
  # mutate(arp = rebate_checks_arp + other_direct_aid + health_grants_arp + non_health_grants + other_vulnerable + federal_ui_arp + state_ui_arp + aid_to_small_businesses,
  #        arp_post_mpc_minus_neutral = rebate_checks_arp_post_mpc_minus_neutral + other_direct_aid_post_mpc_minus_neutral + federal_ui_arp_post_mpc_minus_neutral + state_ui_arp_post_mpc_minus_neutral + other_vulnerable_post_mpc_minus_neutral + non_health_grants_post_mpc_minus_neutral + aid_to_small_businesses_post_mpc_minus_neutral ) %>% 
  transfers_contributions() %>% 
  sum_transfers_contributions() %>% 
  sum_taxes_transfers() %>%
  add_social_benefit_components() %>%
  get_fiscal_impact() %>%
  arrange(date, recession, fiscal_impact, fiscal_impact_moving_average,
          federal_cont, state_local_cont,
          taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont)

  
