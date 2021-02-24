library(targets)
library(tarchetypes)
source('R/packages.R')
source('R/functions.R')
library('lubridate')

current_month <- fim::get_current_month()
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

# End this file with a list of target objects.
plan <-
  tar_plan(
    cbo_projections_raw = load_cbo_projections(),
    fmap = read_xlsx(here('data/raw/nhe_fmap.xlsx')),
    historical = load_national_accounts(),
    last_hist_date = get_last_hist_date(historical),
    last_proj_date = last_hist_date + lubridate::years(2),
    cbo_projections = load_cbo_projections() %>%
      cola_adjustment() %>% 
      smooth_budget_series() %>%
      federal_transfers_growth_rates() %>%
      health_outlays_growth_rates() %>%
      alternative_tax_scenario() %>%
      implicit_price_deflators() %>%
      state_taxes() %>%
      growth_rates(),
    unemployment_insurance_override = load_unemployment_insurance_override(),
    components = get_components_names(),
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
    ,
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
        components_growth_rates() %>%
        create_projections() %>%
        medicaid_reallocation()

# ,
# fim_no_addons =target(  fim_create(projections) %>%
#                           mutate(
#                             federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)
#                           ) %>%
#                           contributions_purchases_grants() %>%
#                           total_purchases() %>%
#                           mutate(federal_cont = federal_cont - federal_grants_cont,
#                                  state_local_cont = state_local_cont + federal_grants_cont) %>%
#                           remove_social_benefit_components() %>%
#                           taxes_transfers_minus_neutral() %>%
#                           calculate_mpc('subsidies') %>%
#                           calculate_mpc('health_outlays') %>%
#                           calculate_mpc('social_benefits') %>%
#                           calculate_mpc('unemployment_insurance') %>%
#                           calculate_mpc('rebate_checks') %>%
#                           calculate_mpc('noncorp_taxes') %>%
#                           calculate_mpc('corporate_taxes') %>%
#                           taxes_contributions() %>%
#                           sum_taxes_contributions() %>%
#                           transfers_contributions() %>%
#                           sum_transfers_contributions() %>%
#                           sum_taxes_transfers() %>%
#                           add_social_benefit_components() %>%
#                           get_fiscal_impact() %>%
#                           arrange(
#                             date,
#                             recession,
#                             fiscal_impact,
#                             fiscal_impact_moving_average,
#                             federal_cont,
#                             state_local_cont,
#                             taxes_transfers_cont,
#                             federal_taxes_transfers_cont,
#                             state_taxes_transfers_cont
#                           )
# )
    # fim_no_addons =
    #   target(
    #     fim %>%
    #       filter(date >= '2020-03-31') %>%
    #       transmute(
    #         date  = yearquarter(date),
    #         state_health_outlays = state_health_outlays -
    #           add_state_health_outlays,
    #         state_social_benefits = state_social_benefits -
    #           add_state_social_benefits,
    #         state_noncorp_taxes = state_noncorp_taxes -
    #           add_state_noncorp_taxes,
    #         state_corporate_taxes = state_corporate_taxes -
    #           add_state_corporate_taxes,
    #         federal_health_outlays = federal_health_outlays -
    #           add_federal_health_outlays,
    #         federal_social_benefits = federal_social_benefits -
    #           add_federal_social_benefits,
    #         federal_subsidies = federal_subsidies -
    #           add_federal_subsidies,
    #         federal_cgrants = federal_cgrants -
    #           add_federal_cgrants
    #       ) %>%
    #       pivot_longer(-date, names_to = 'variables', values_to = 'values_one') %>%
    #       pivot_longer(date) %>% pivot_wider(names_from = value, values_from  = values_one) %>%
    #       select(-name)
    #   ), 
    # fim_nipa_consistent =
    #   target(
    #     fim_create(nipa_projections) %>%
    #       add_factors(last_date = last_hist_date) %>%
    #       fim_calculations()
    #   ),
    # fim_interactive = fim %>%
    #   prepare_interactive(),
    # nipa_interactive =
    #   target(fim_nipa_consistent %>% prepare_interactive()),
    # output = target({
    #   dir.create(glue::glue('results/{current_month}'))
    #   write_xlsx(fim, file_out(
    #     !!paste0('results/', current_month, '/fim-', current_month, '.xlsx')
    #   ))
    #   write.xlsx(fim_no_addons,
    #              glue('results/{current_month}/fim-no-addons.xlsx'))
    #   write.csv(fim_interactive,
    #             glue::glue('results/{current_month}/fim_interactive.csv'))
    #   write.xlsx(
    #     nipa_interactive,
    #     glue::glue('results/{current_month}/nipa_interactive.xlsx'),
    #     sheetName = 'contributions'
    #   )
    #   write.csv(projections, file_out(
    #     !!glue::glue(
    #       'results/{fim::get_current_month()}/projections-{fim::get_current_month()}.csv'
    #     )
    #   ))
    # }),
    # contributions = fim %>%
    #   select(
    #     date,
    #     fiscal_impact,
    #     fiscal_impact_moving_average,
    #     ends_with('cont'),
    #     recession
    #   ) %>%
    #   mutate(date = lubridate::as_date(date)) %>%
    #   filter(date > '2000-01-01' & date <= '2022-12-31') #,
    # max_y = contributions %>%
    #   select(fiscal_impact) %>%
    #   max() %>% ceiling(),
    # hutchins_logo = knitr::include_graphics(file.path(
    #   here::here(), "images", "HC_NEW_BROOKINGS_RGB.jpg"
    # )),
    # fim_report =
    #   rmarkdown::render(
    #     knitr_in('Fiscal-Impact.Rmd'),
    #     output_file = file_out(!!file.path(here(), 'results', get_current_month(), 'reports', 'Fiscal-Impact.pdf')),
    #     quiet  = TRUE
    #   ),
    # fim_report_expanded =
    #   rmarkdown::render(
    #     knitr_in('Fiscal-Impact-Expanded.Rmd'),
    #     output_file = file_out(!!file.path(here(), 'results', get_current_month(),  'reports', 'Fiscal-Impact-Expanded.pdf')),
    #     quiet  = TRUE
    #   )
    # compare_update =
    #   rmarkdown::render(
    #     knitr_in('compare-update.Rmd'),
    #     output_file = file_out(!!file.path(here(), 'results', get_current_month(), 'reports', 'Update-Comparison.html')),
    #     quiet = TRUE
    #   )
    
    # fim_report_expanded = target({
    #   if(file.exists(!!glue::glue('reports/{current_month}/Fiscal-Impact-Expanded.pdf')) == FALSE){
    #   rmarkdown::render(
    #     drake::knitr_in("reports/Fiscal-Impact-Expanded.Rmd"),
    #     output_dir = 'reports',
    #     output_file = 'reports/Fiscal-Impact-Expanded.pdf',
    #     quiet = TRUE
    #   )
    #
    #   filesstrings::file.move('reports/Fiscal-Impact-Expanded.pdf',
    #                           !!glue::glue('reports/{current_month}'),
    #                           overwrite = TRUE)
    #   }
    #   else {
    #
    #   }
    # }),
    # state_local_employment_raw = readxl::read_xlsx(file_in('data/raw/haver/state_local_employment.xlsx')),
    # state_local_employment = target(state_local_employment_raw %>%
    #   mutate(date = lubridate::as_date(date)) %>%
    #   rename(state_employment = lasgova,
    #          local_employment = lalgova,
    #          construction = cpgs) %>%
    #   mutate(state_local_employment = state_employment + local_employment) %>%
    #   filter(date > lubridate::today() - lubridate::years(1))
    #   ),
    # state_local_employment_report = target({
    #   current_month = fim::get_current_month()
    #   rmarkdown::render(
    #     knitr_in("reports/state_local_employment.Rmd"),
    #     output_dir = 'reports',
    #     output_file = 'reports/state_local_employment.html',
    #     quiet = TRUE
    #   )
    #   file_out(!!glue::glue('reports/{current_month}/state_local_employment.html'))
    #   filesstrings::file.move('reports/state_local_employment.html',
    #                           !!glue::glue('reports/{current_month}'),
    #                           overwrite = TRUE)
    # }
    # ),
    # compare_contributions = target(
    #   {
    #     render(
    #       knitr_in("reports/compare-update.Rmd"),
    #       output_dir = 'reports',
    #       output_file = 'reports/Update-Comparison.html',
    #       quiet = TRUE
    #     )
    #     file_out(!!glue::glue('reports/{current_month}/Update-Comparison.html'))
    #     file.move('reports/Update-Comparison.html',
    #               !!glue::glue('reports/{current_month}'),
    #               overwrite = TRUE)
    #   }
    # )
  )
