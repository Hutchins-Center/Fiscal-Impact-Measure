create_projections <- function(df, last_date){
components <- get_components_names()
  df %>%  
    mutate(forecast_period = if_else(date <= '2020-09-30', 0, 1)) %>%
    mutate(historical = if_else(date > last_date, 0, 1)) %>%
    make_cumulative_growth_rates() %>%
    fill(components) %>%
    make_forecasts() %>% 
    sum_projections(gtfp, gftfp, gstfp) %>%
    sum_projections(yptx, gfrpt, gsrpt) %>%
    sum_projections(ytpi, gfrpri, gsrpri) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(yctlg, gfrcp, gsrcp) %>%
    sum_projections(gsub, gfsub, gssub)
}

components_growth_rates <- function(df){
  df %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth() 
}

fim_calculations <- function(df){
  df %>%  
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

}



taxes_transfers_minus_zero <- function(df){
  df %>% 
    dplyr::mutate(dplyr::across(.cols = all_of(all_taxes_transfers()),
                                .fns = ~. -dplyr::lag(.) * (1 + pi_pce),
                  .names = ''))
}

contribution_zero <- function(.data, var){
  var <- ensym(var) # quote expression
  var_string <- rlang::as_string(enexpr(var)) # convert quoted expression to string
  deflator_string <- paste0(var_string, "_pi") # create string with the name of relevant deflator
  deflator <- rlang::sym(deflator_string) # convert deflator string to symbol
  
  ## Calculate contribution
  .data %>%
    mutate(
      "{{ var }}_cont" := 400 * ({{ var }} - (1  + !!(deflator) ) * lag({{ var }}) ) / lag(gdp)
    ) %>%
    select(date, !!paste0(var_string, "_cont"))
}

contributions_purchases_grants_zero <- function (df){
  map(alist(federal_nom, state_local_nom, federal_cgrants, 
            federal_igrants), ~contribution_zero(df, !!.x)) %>% reduce(left_join) %>% 
    left_join(df, .)
}

all_taxes_transfers <- function(){
  taxes_transfers <- c("subsidies","health_outlays", "social_benefits",
                       "noncorp_taxes", "corporate_taxes", 'rebate_checks', 
                       'unemployment_insurance')
  government_level <- c('federal', 'state')
  all_taxes_transfers <- c(glue::glue('{taxes_transfers}'), glue::glue('federal_{taxes_transfers}'),
                           glue::glue('state_{taxes_transfers}'))
  return(all_taxes_transfers)
}
