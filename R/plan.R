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
    prepare_projections = full_join(historical,
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
      millions_to_billions() %>%
      unemployment_insurance_reallocation() %>%
      fmap_share_old() %>%
      purchases_growth() %>%
      transfers_growth() %>%
      health_growth() %>%
      subsidies_growth() %>%
      grants_growth() %>%
      deflators_growth() %>%
      mutate(historical = if_else(date > last_hist_date, 0, 1)), 
      projections = prepare_projections %>%
        mutate(forecast_period = if_else(date <= '2020-09-30', 0, 1)) %>%
        make_cumulative_growth_rates() %>%
        fill(components) %>%
        make_forecasts() %>% 
        sum_projections(gtfp, gftfp, gstfp) %>%
        sum_projections(yptx, gfrpt, gsrpt) %>%
        sum_projections(ytpi, gfrpri, gsrpri) %>%
        sum_projections(grcsi, gfrs, gsrs) %>%
        sum_projections(grcsi, gfrs, gsrs) %>%
        sum_projections(yctlg, gfrcp, gsrcp) %>%
        sum_projections(gsub, gfsub, gssub) %>%
        medicaid_reallocation(),
    fim = fim_create(projections) %>%
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
              taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont),
    fim_interactive = fim %>%
      prepare_interactive(),
    output = {
      dir.create(glue(file_out('results/{get_current_month()}')))
      writexl::write_xlsx(fim, glue(file_out("results/{get_current_month()}/fim-{get_current_month()}.xlsx")))
      write.csv(fim_interactive, glue(file_out('results/{get_current_month()}/fim_interactive.csv')))
      write.csv(projections, glue(file_out('results/{get_current_month()}/projections-{get_current_month()}.csv')))
    },
    contributions = fim %>%
      select(date, fiscal_impact, fiscal_impact_moving_average,
             ends_with('cont'), recession) %>%
      mutate(date = lubridate::as_date(date)) %>%
      filter(date > '2000-01-01' & date <= '2022-12-31'),
    report = rmarkdown::render(
      knitr_in("reports/drake-report.Rmd"),
      output_file = file_out("drake-report.pdf"),
      quiet = TRUE
    ),
    state_local_employment_raw = readxl::read_xlsx(file_in('data/raw/haver/state_local_employment.xlsx')),
    state_local_employment = state_local_employment_raw %>%
      mutate(date = lubridate::as_date(date)) %>%
      rename(state_employment = lasgova, 
             local_employment = lalgova) %>%
      mutate(state_local_employment = state_employment + local_employment)
  )

