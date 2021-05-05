fim =
  fim_create(projections) %>%
  # projections %>%
  mutate(id =  if_else(date <= last_hist_date, 'historical', 'projection')) %>%
  add_factors() %>%
  override_projections() %>%
  fill_overrides() %>% 
  mutate(date2 = yearquarter(date)) %>% 
  as_tsibble(index = date2) %>% 
  full_join(read_xlsx('data/pandemic-legislation/arp_summary.xlsx') %>% 
              mutate(date2 = yearquarter(date)) %>% filter(date2 > yearquarter('2021 Q1')), by = 'date2') %>% 
  rename(date = date.x) %>% 
  as_tibble() %>% 
  mutate(federal_cgrants = coalesce(federal_cgrants_override, federal_cgrants)) %>%
  contributions_purchases_grants() %>%
  total_purchases() 

  # remove_social_benefit_components() %>%
 fim %>%  taxes_transfers_minus_neutral() %>%
  mutate(across(where(is.numeric),
                ~ coalesce(.x, 0))) %>% 
  mutate(across(.cols = all_of(c('rebate_checks_arp', 'other_direct_aid', 
                                 'health_grants_arp', 'non_health_grants',
                                 'other_vulnerable', 'federal_ui_arp', 'state_ui_arp', 'aid_to_small_businesses')),
                .fns = ~ .x - dplyr::lag(.x) * (1 + gdppoth + pi_pce),
                .names = '{.col}_minus_neutral')) %>% 
   calculate_mpc('rebate_checks') %>% 
   mutate(rebate_checks_post_mpc=mpc_rebate_checks(rebate_checks_minus_neutral) )%>%
  fim %>%  filter(date >= "2020-03-31") %>% select(date, contains("rebate")) %>% View()
 
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
         federal_nom = federal_nom + non_health_grants,
         federal_nom_cont = federal_nom_cont + non_health_grants_cont,
         federal_nom_new = federal_nom - non_health_grants,
         federal_nom_new_cont = federal_nom_cont - non_health_grants_cont,
         federal_cont_no_arp = federal_cont, 
         federal_cont = federal_cont + non_health_grants_cont,
         purchases_cont = purchases_cont + non_health_grants_cont) %>% 
  mutate(across(.cols = all_of(c('federal_ui_arp', 'state_ui_arp', 'other_vulnerable') %>% paste0('_minus_neutral')),
                .fns = ~ mpc_vulnerable_arp(.x),
                .names = '{.col}_post_mpc'),
         across(.cols = all_of(c('rebate_checks_arp','other_direct_aid') %>% paste0('_minus_neutral')),
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
  mutate(fiscal_impact = federal_cont + state_local_cont + taxes_cont + state_transfers_cont +  federal_transfers_cont_no_arp + federal_ui_arp_cont + state_ui_arp_cont + rebate_checks_arp_cont + aid_to_small_businesses_cont + health_grants_arp_cont + other_direct_aid_cont + other_vulnerable_cont,
         arp_cont =  health_grants_arp_cont + non_health_grants_cont +
           federal_ui_arp_cont + rebate_checks_arp_cont + other_direct_aid_cont + other_vulnerable_cont + aid_to_small_businesses_cont) %>% 
  mutate(federal_unemployment_insurance = federal_unemployment_insurance + federal_ui_arp, federal_unemployment_insurance_cont = federal_unemployment_insurance_cont + federal_ui_arp_cont, state_unemployment_insurance = state_unemployment_insurance + state_ui_arp, state_unemployment_insurance_cont = state_unemployment_insurance_cont + state_ui_arp_cont, federal_purchases_with_grants = federal_nom + federal_cgrants + federal_igrants, state_purchases_with_grants = state_local_nom - federal_cgrants - federal_igrants) %>% 
  
  
  arrange(date, recession, fiscal_impact, fiscal_impact_moving_average,
          federal_cont, state_local_cont,
          taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont)

fim <- fim %>% mutate(rebate_checks = rebate_checks + rebate_checks_arp)

fim %>% filter(date > "2020-06-30") %>% select(date, fiscal_impact)  



# projections %>% fim_create()  %>%  select(date, state_health_outlays,
#  state_social_benefits,
#  state_noncorp_taxes,
#  state_corporate_taxes,
#  federal_health_outlays,
#  federal_social_benefits,
#  federal_subsidies,
#  federal_cgrants) %>% filter(date > "2020-03-31") %>%
#  write_xlsx('data/add-ons/fim_no_addons.xlsx')

write_xlsx(fim, 'results/4-2021/fim-4-2021.xlsx')


