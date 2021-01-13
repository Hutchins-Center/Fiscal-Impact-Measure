current_month <- get_current_month()
components <- get_components_names()
plan <- 
  drake_plan(
    cbo_projections_raw = load_cbo_projections(),
    fmap = read_xlsx(here('data/raw/nhe_fmap.xlsx')),
    historical = target(load_haver_data()),
    last_hist_date = get_last_hist_date(historical),
    last_proj_date = last_hist_date + lubridate::years(2),
    cbo_projections = cbo_projections_raw %>% cola_adjustment() %>%
      smooth_budget_series() %>%
      federal_transfers_growth_rates() %>%
      health_outlays_growth_rates() %>%
      alternative_tax_scenario() %>%
      implicit_price_deflators() %>%
      state_taxes() %>%
      growth_rates(),
    unemployment_insurance_override = load_unemployment_insurance_override(),
    components = get_components_names(),
    combined_data = target(full_join(historical,
                            cbo_projections %>%
                              dplyr::select(date, tidyselect::contains('_g')),
                            by = 'date') %>%
      left_join(unemployment_insurance_override) %>%
      mutate(
        across(
          .cols = ends_with('override'),
          .fns = ~ if_else(is.na(.x), 0, .x)
        )
      ) %>%
      millions_to_billions() 
      ), 
      projections = target(
      combined_data %>% 
      unemployment_insurance_reallocation() %>%
      fmap_share_old() %>%
      components_growth_rates() %>%
      create_projections(last_date = last_hist_date) %>%
      medicaid_reallocation()
     ), 
      nipa_projections = target(
      combined_data %>%
        fmap_share_old() %>%
        components_growth_rates() %>%
        create_projections(last_date = last_hist_date) %>%
        mutate(yfptmd = 0, ysptmd = yptmd, 
               gftfpnet = gftfp, gstfpnet = gstfp)
    ),
    nipa_purchases_contributions = target(
      nipa_projections %>%
        fim_create() %>% 
        add_factors() %>%
        override_projections() %>%
        contributions_purchases_grants_zero() %>%
        total_purchases() %>%
        select(date, ends_with('cont'))
    ),
    fim = target(fim_create(projections) %>%
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
    ),
    fim_nipa_consistent = 
      target(
          fim_create(nipa_projections) %>%
          fim_calculations()
      ),
      fim_interactive = fim %>%
      prepare_interactive(),
      nipa_interactive = 
      target(
        fim_nipa_consistent %>% prepare_interactive()
      ),
    output = target({
      dir.create(glue('results/{current_month}'))
      write_xlsx(fim, file_out(!!paste0('results/', current_month, '/fim-', current_month, '.xlsx')))
      write.csv(fim_interactive, glue('results/{current_month}/fim_interactive.csv'))
      write.xlsx(nipa_interactive, glue('results/{current_month}/nipa_interactive.xlsx'), sheetName = 'contributions')
      write.csv(projections, file_out(!!glue('results/{get_current_month()}/projections-{get_current_month()}.csv')))
    }),
    contributions = fim %>%
      select(date, fiscal_impact, fiscal_impact_moving_average,
             ends_with('cont'), recession) %>%
      mutate(date = lubridate::as_date(date)) %>%
      filter(date > '2000-01-01' & date <= '2022-12-31'),
    max_y = contributions %>% 
      select(fiscal_impact) %>% 
      max() %>% ceiling(),
    recession_shade = contributions %>%
                      get_recession_shade(),
    hutchins_logo = knitr::include_graphics(file.path(here::here(),"images","HC_NEW_BROOKINGS_RGB.jpg")),
    fim_report = {
          rmarkdown::render(
          drake::knitr_in("reports/Fiscal-Impact.Rmd"),
          output_dir = 'reports',
          output_file = 'Fiscal-Impact.pdf',
          quiet = TRUE
        )
      filesstrings::file.move('reports/Fiscal-Impact.pdf',
                              !!glue('reports/{get_current_month()}'),
                              overwrite = TRUE)
      },
    fim_report_expanded = target({
      if(file.exists(!!glue('reports/{get_current_month()}/Fiscal-Impact-Expanded.pdf')) == FALSE){
      rmarkdown::render(
        drake::knitr_in("reports/Fiscal-Impact-Expanded.Rmd"),
        output_dir = 'reports',
        output_file = 'Fiscal-Impact-Expanded.pdf',
        quiet = TRUE
      )
        
      filesstrings::file.move('reports/Fiscal-Impact-Expanded.pdf',
                              !!glue('reports/{get_current_month()}'),
                              overwrite = TRUE)
      }
      else {
        
      }
    }),
    state_local_employment_raw = readxl::read_xlsx(file_in('data/raw/haver/state_local_employment.xlsx')),
    state_local_employment = target(state_local_employment_raw %>%
      mutate(date = lubridate::as_date(date)) %>%
      rename(state_employment = lasgova, 
             local_employment = lalgova,
             construction = cpgs) %>%
      mutate(state_local_employment = state_employment + local_employment) %>%
      filter(date > lubridate::today() - lubridate::years(1))
      ),
    state_local_employment_report = target({
      current_month = get_current_month()
      rmarkdown::render(
        knitr_in("reports/state_local_employment.Rmd"),
        output_dir = 'reports',
        output_file = 'state_local_employment.html',
        quiet = TRUE
      )
      file_out(!!glue('reports/{current_month}/state_local_employment.html'))
      filesstrings::file.move('reports/state_local_employment.html',
                              !!glue('reports/{current_month}'), 
                              overwrite = TRUE)
    }
    ),
    compare_contributions = target(
      {
        render(
          knitr_in("reports/compare-update.Rmd"),
          output_dir = 'reports',
          output_file = 'Update-Comparison.pdf',
          quiet = TRUE
        )
        file_out(!!glue('reports/{current_month}/Update-Comparison.pdf'))
        file.move('reports/Update-Comparison.pdf',
                  !!glue('reports/{current_month}'))
      }
    )
  )
    
      
