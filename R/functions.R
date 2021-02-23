create_override <- function(df, var, start, end, values){
  override <- 
    tibble(date = df %>%
             filter(date >= start & date <= end) %>%
             pull(date),
           '{{var}}' := values
    )
  df %>%
    rows_update(override, by = 'date')
}

create_projections <- function(df){
components <- get_components_names()
  df %>%  
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


add_factors_v2 <- function(df, last_date){
  #load add factor file
  add_factors <- readxl::read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                                    sheet = "FIM Add Factors") %>%
    mutate(
      date = lubridate::as_date(date)
    ) 
  df %>% 
    dplyr::full_join(add_factors %>% dplyr::select(-tidyselect::ends_with('override')) %>% 
                       filter(date > last_date),
                     by = "date") %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::starts_with('add_'),
      .fns = ~ if_else(is.na(.x), 
                       0,
                       .x)
    )
    ) %>%
    dplyr::mutate(
      #calculate new variables by adding the add factors
      state_health_outlays  = state_health_outlays + add_state_health_outlays,
      state_social_benefits  = state_social_benefits + add_state_social_benefits,
      state_noncorp_taxes  =  state_noncorp_taxes + add_state_noncorp_taxes,
      state_corporate_taxes  = state_corporate_taxes + add_state_corporate_taxes,
      
      federal_health_outlays  = federal_health_outlays + add_federal_health_outlays,
      federal_social_benefits  = federal_social_benefits + add_federal_social_benefits,
      # federal_noncorp_taxes  = federal_noncorp_taxes + add_federal_noncorp_taxes,
      # federal_corporate_taxes  = federal_corporate_taxes + add_federal_corporate_taxes,
      federal_subsidies  = federal_subsidies + add_federal_subsidies,
      federal_cgrants = federal_cgrants + add_federal_cgrants,
      
      #new category totals
      health_outlays  = state_health_outlays  + federal_health_outlays ,
      social_benefits  = state_social_benefits  + federal_social_benefits ,
      noncorp_taxes  = state_noncorp_taxes  + federal_noncorp_taxes ,
      corporate_taxes  = state_corporate_taxes  + federal_corporate_taxes ,
      subsidies   = state_subsidies + federal_subsidies,
      state_local_nom = state_local_nom + add_state_purchases,
      federal_nom = add_federal_purchases + federal_nom,
      federal_rebate_checks = federal_rebate_checks + add_rebate_checks,
      rebate_checks = rebate_checks + add_rebate_checks
    )
}



fim_plot <- 
  function(df, title,  hist_end){
    df %>%
      ggplot() +
      geom_bar(aes(x = date, y = value, fill = variable),
               stat = 'identity', width = 50) +
      geom_line(
        aes(x = date,
            y = fiscal_impact_moving_average,
            colour = "4-quarter moving-average")
      ) +
      geom_point(
        aes(x = date,
            y = fiscal_impact_moving_average,
            colour = "4-quarter moving-average"), size = 1
      ) +
      labs(
        title = glue("**Hutchins Center Fiscal Impact Measure: {title}**"),
        x = '',
        y = '',
        subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
        caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
      geom_richtext(aes(x = Sys.Date()+350,
                        y = 16), 
                    label = "Projection",
                    cex = 2, 
                    fill = NA, label.color = NA, # remove background and outline
      ) +
      annotate("rect", xmin = hist_end + 40, xmax = lubridate::as_date('2022-12-31'),
               ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
      scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = '%Y',
                   expand = c(0,0)) + 
      scale_color_manual(" ", 
                         values=c("4-quarter moving-average" ="black",
                                  "4-quarter moving-average" ="black")) +
      uni.theme() 
  }



fim_update <-
  function (df)
  {
    df %>% transmute(
      date = date,
      historical = historical,
      gdp = gdp,
      gdppot = q_g(gdppothq) + q_g(jgdp),
      gdppoth = q_g(gdppothq),
      pi_gdp = q_g(jgdp),
      pce = c,
      pi_pce = q_g(jc),
      recession = if_else(recessq ==
                            -1, 0, recessq),
      federal_nom = gf,
      federal_cgrants_gross = gfeg,
      federal_health_grants = gfeghhx,
      federal_medicaid_grants = yfptmd,
      federal_cgrants = federal_cgrants_gross - federal_medicaid_grants,
      federal_igrants = gfeigx,
      pi_federal = q_g(jgf),
      state_local_nom = gs,
      pi_state_local = q_g(jgs),
      pi_state_local_c = q_g(jgse),
      pi_state_local_i = q_g(jgsi),
      federal_nom_pi = pi_federal,
      state_local_nom_pi = pi_state_local,
      federal_cgrants_pi = pi_state_local_c,
      federal_igrants_pi = pi_state_local_i,
      medicare = yptmr,
      medicaid = yptmd,
      health_outlays = medicare + medicaid,
      gtfp,
      social_benefits_gross = gtfp,
      social_benefits = social_benefits_gross - health_outlays,
      personal_taxes = yptx,
      payroll_taxes = grcsi,
      production_taxes = ytpi,
      noncorp_taxes = personal_taxes + production_taxes + payroll_taxes,
      corporate_taxes = yctlg,
      subsidies = gsub,
      rebate_checks = if_else(is.na(gftfpe),
                              0, gftfpe),
      federal_medicaid = yfptmd,
      federal_health_outlays = medicare +
        federal_medicaid,
      
      federal_rebate_checks = rebate_checks,
      federal_social_benefits = gftfpnet -
        federal_health_outlays,
      federal_personal_taxes = gfrpt,
      federal_payroll_taxes = gfrs,
      federal_production_taxes = gfrpri,
      federal_noncorp_taxes = federal_personal_taxes + federal_payroll_taxes +
        federal_production_taxes,
      federal_corporate_taxes = gfrcp,
      federal_subsidies = gfsub,
      state_medicaid = medicaid -
        federal_medicaid,
      state_health_outlays = state_medicaid,
      state_social_benefits_gross = gstfp,
      state_rebate_checks = 0,
      state_social_benefits = state_social_benefits_gross -
        state_health_outlays - federal_medicaid_grants,
      state_personal_taxes = gsrpt,
      state_payroll_taxes = gsrs,
      state_production_taxes = gsrpri,
      state_noncorp_taxes = state_personal_taxes + state_payroll_taxes +
        state_production_taxes,
      state_corporate_taxes = gsrcp,
      unemployment_insurance,
      federal_unemployment_insurance,
      state_unemployment_insurance,
      state_subsidies = gssub
    )
  }
