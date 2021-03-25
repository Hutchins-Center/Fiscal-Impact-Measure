fim %>%
  select(date, state_health_outlays, 
         state_social_benefits,
         state_noncorp_taxes,
         state_corporate_taxes,
         state_subsidies,
         federal_health_outlays,
         federal_social_benefits,
         federal_noncorp_taxes,
         federal_corporate_taxes,
         federal_subsidies,
         federal_cgrants) %>%
  filter(date > '2019-12-31') %>%
  write_xlsx(., here('development', 'features', 'nipa-consistent-FIM',
                     'results', 'fim-noaddons.xlsx'))
