current_month <- fim::get_current_month()
components <- get_components_names()
plan <-
  drake_plan(
    cbo_projections_raw = target(load_cbo_projections()),
    fmap = read_xlsx(here('data/raw/nhe_fmap.xlsx')),
    historical = target(load_haver_data()),
    last_hist_date = target(get_last_hist_date(historical)),
    last_proj_date = target(last_hist_date + lubridate::years(2)),
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
    combined_data = target(
      full_join(
        historical,
        cbo_projections %>%
          rename(federal_medicaid = yptmd,medicare =  yptmr) %>%
          dplyr::select(date, federal_medicaid, medicare, federal_unemployment_insurance, state_unemployment_insurance, tidyselect::contains('_g')),
        by = 'date'
      ) %>%
        left_join(unemployment_insurance_override) %>%
        mutate(across(
          .cols = ends_with('override'),
          .fns = ~ if_else(is.na(.x), 0, .x)
        )) %>%
        millions_to_billions()
    ),
    
    projections = target(
      combined_data %>%
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
        components_growth_rates() %>%
        create_projections() %>%
        medicaid_reallocation()
    ),
    updated_projections = 
      target(
        combined_data %>%
          select(-contains('override')) %>%
          mutate(
            across(c('federal_unemployment_insurance',  'yptue', 'yptuc', 'yptup', 'yptolm'),
                   ~ coalesce(.x, 0)),
            federal_unemployment_insurance = federal_unemployment_insurance + yptue + yptuc + yptup + yptolm,
            gftfp = gftfp - yptu + federal_unemployment_insurance,
            gstfp = gstfp + yptu - federal_unemployment_insurance,
            state_unemployment_insurance = coalesce(state_unemployment_insurance, yptu - federal_unemployment_insurance),
            unemployment_insurance = federal_unemployment_insurance + state_unemployment_insurance) %>%
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
          components_growth_rates() %>%
          create_projections() %>%
          mutate(yfptmd = coalesce(gfeghdx, federal_medicaid),
                 ysptmd = yptmd - yfptmd, 
                 gfegnet = gfeg - yfptmd, gstfpnet = gstfp - yfptmd, gftfpnet = gftfp + yfptmd)

      ),
    updated_fim = target(
      updated_projections %>%
        select(-ends_with('override')) %>%
        fim_update() %>% 
        mutate(federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)) %>%
        contributions_purchases_grants() %>%
        total_purchases() %>%
        mutate(federal_cont = federal_cont - federal_grants_cont,
               state_local_cont = state_local_cont + federal_grants_cont) %>%
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
        arrange(
          date,
          recession,
          fiscal_impact,
          fiscal_impact_moving_average,
          federal_cont,
          state_local_cont,
          taxes_transfers_cont,
          federal_taxes_transfers_cont,
          state_taxes_transfers_cont
        )
    ),
    fim = target(
      fim_create(projections) %>%
        add_factors(last_date = last_hist_date) %>%
        override_projections() %>%
        mutate(
          federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)
        ) %>%
        fill_overrides() %>%
        contributions_purchases_grants() %>%
        total_purchases() %>%
        mutate(federal_cont = federal_cont - federal_grants_cont,
               state_local_cont = state_local_cont + federal_grants_cont) %>%
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
        arrange(
          date,
          recession,
          fiscal_impact,
          fiscal_impact_moving_average,
          federal_cont,
          state_local_cont,
          taxes_transfers_cont,
          federal_taxes_transfers_cont,
          state_taxes_transfers_cont
        )
    ),
    fim_no_addons =target(  fim_create(projections) %>%
                              mutate(
                                federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)
                              ) %>%
                              contributions_purchases_grants() %>%
                              total_purchases() %>%
                              mutate(federal_cont = federal_cont - federal_grants_cont,
                                     state_local_cont = state_local_cont + federal_grants_cont) %>%
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
                              arrange(
                                date,
                                recession,
                                fiscal_impact,
                                fiscal_impact_moving_average,
                                federal_cont,
                                state_local_cont,
                                taxes_transfers_cont,
                                federal_taxes_transfers_cont,
                                state_taxes_transfers_cont
                              )
    ),

    output = target({
      dir.create(glue::glue('results/{current_month}'))
      write_xlsx(fim, file_out(
        !!paste0('results/', current_month, '/fim-', current_month, '.xlsx')
      ))
      write.xlsx(fim_no_addons,
                 glue('results/{current_month}/fim-no-addons.xlsx'))
      write.csv(projections, file_out(
        !!glue::glue(
          'results/{fim::get_current_month()}/projections-{fim::get_current_month()}.csv'
        )
      ))
    })
  
  )
