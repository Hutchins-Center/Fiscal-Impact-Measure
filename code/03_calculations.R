
# 5 Construct FIM data frame for calculations -----------------------------------------------------------
fim_create <- function(df){
  df %>%
  transmute(date = date,
         # REFERENCE VARIABLES
         gdp = gdp, # nominal gdp
         gdppot = q_g(gdppothq) + q_g(jgdp) , #  nominal potential output growth
         gdppoth = q_g(gdppothq), # real potential output growth
         pi_gdp = q_g(jgdp), # gdp "deflator"
         pce = c, # nominal consumption
         pi_pce = q_g(jc),
         recession = if_else(recessq == -1,
                             0,
                             recessq),   
         # GRANTS & NOMINAL SPENDING
         ## Federal
         federal_nom = gf, # SPENDING
         ## GRANTS
         federal_cgrants_gross  = gfeg,
         federal_health_grants = gfeghhx,
         federal_medicaid_grants = yfptmd, 
         federal_cgrants = federal_cgrants_gross - federal_medicaid_grants, # Federal (non-health) grants in aid to states
         federal_igrants = gfeigx, # Federal capital grants in aid to states, nominal
         pi_federal = q_g(jgf),
         ## State
         state_local_nom = gs ,
         pi_state_local = q_g(jgs) ,
         pi_state_local_c = q_g(jgse) ,
         pi_state_local_i = q_g(jgsi),
         federal_nom_pi = pi_federal,
         state_local_nom_pi = pi_state_local,
         federal_cgrants_pi = pi_state_local_c,
         federal_igrants_pi = pi_state_local_i,
         # TAXES AND TRANSFERS
         ## Total
         medicare = yptmr,
         medicaid = yptmd,
         health_outlays = medicare + medicaid, # Medicare + Medicaid
         unemployment_insurance = yptu,
         gtfp,
         social_benefits_gross = gtfp,
         social_benefits = social_benefits_gross - health_outlays, # Social benefits net health outlays
         personal_taxes = yptx,
         payroll_taxes = grcsi,
         production_taxes = ytpi,
         noncorp_taxes = personal_taxes + production_taxes + payroll_taxes, # alternative
         corporate_taxes = yctlg,
         subsidies  = gsub,
         rebate_checks = if_else(is.na(gftfpe), 0, gftfpe),
         ## Federal
         federal_medicaid = yfptmd,
         federal_health_outlays = medicare + federal_medicaid,
         federal_unemployment_insurance = federal_unemployment_insurance_override,
         federal_rebate_checks = rebate_checks,
         federal_social_benefits = gftfpnet - federal_health_outlays,
         federal_personal_taxes = gfrpt,
         federal_payroll_taxes = gfrs,
         federal_production_taxes  = gfrpri,
         federal_noncorp_taxes = federal_personal_taxes + federal_payroll_taxes + federal_production_taxes,
         federal_corporate_taxes = gfrcp,
         federal_subsidies = gfsub,
         ## State
         state_medicaid = medicaid - federal_medicaid,
         state_health_outlays = state_medicaid,
         state_social_benefits_gross = gstfp,
         state_unemployment_insurance = unemployment_insurance - federal_unemployment_insurance,
         state_rebate_checks = 0,
         state_social_benefits = state_social_benefits_gross - state_health_outlays - federal_medicaid_grants, # no medicare at state and local level
         state_personal_taxes = gsrpt,
         state_payroll_taxes = gsrs,
         state_production_taxes  = gsrpri,
         state_noncorp_taxes = state_personal_taxes + state_payroll_taxes + state_production_taxes,
         state_corporate_taxes = gsrcp,
         state_subsidies = gssub
         )
}
add_factors <- function(df){
  #load add factor file
  add_factors <- read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                            sheet = "FIM Add Factors") %>%
    mutate(
      date = as_date(date)
    ) 
  df %>% 
    full_join(add_factors %>% select(-ends_with('override')) %>%
                filter(date > last_hist_date),
              by = "date") %>%
    mutate(across(
      .cols = starts_with('add_'),
      .fns = ~ if_else(is.na(.x), 
                       0,
                       .x)
    )
    ) %>%
    mutate(
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
override_projections <- function(df){ 
  override <- read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                         sheet = "FIM Add Factors") %>%
    select(date, ends_with('override')) %>%
    mutate(date = as_date(date)) 
  
  Q2_2020 <- '2020-06-30'
  Q3_2020 <- '2020-09-30'
  last_override <- '2022-12-31'
  
  df %>%
    left_join(override, by = 'date') %>%
    mutate(unemployment_insurance = if_else(date >= Q3_2020 & date <= last_override,
                                            unemployment_insurance_override,
                                            unemployment_insurance),
           federal_unemployment_insurance = if_else(date >= Q2_2020 & date <= last_override,
                                                    federal_unemployment_insurance_override,
                                                    federal_unemployment_insurance),
           state_unemployment_insurance = if_else(date >= Q2_2020 & date <= last_override,
                                                  state_unemployment_insurance_override,
                                                  state_unemployment_insurance),
           federal_cgrants = if_else(date >= Q2_2020 & date <= Q3_2020,
                                     federal_cgrants_override,
                                     federal_cgrants)
    ) 
}
fill_overrides <- function(df){
  overrides <- c('unemployment_insurance', 'federal_unemployment_insurance', 'state_unemployment_insurance')
  df %>%
  mutate(
    across(
      .cols = contains('unemployment_insurance'),
      .fns = ~if_else(is.na(.), 0, .)
    )
  )
}
fim <- 
  fim_create(projections) %>% 
  add_factors() %>%
  override_projections() %>%
  fill_overrides()

# 4.3 Contribution of purchases and grants -------------------------------------------------------------------------------------
contributions_purchases_grants <- function(df){
  map(
    alist(federal_nom, state_local_nom, federal_cgrants, federal_igrants),
    ~ contribution(df, !!.x)
  ) %>%
    reduce(left_join) %>%
    left_join(df, .)
}
total_purchases <- function(df){
  df %>%
    mutate(federal_cont_ex_grants = federal_nom_cont,
           federal_grants_cont = federal_cgrants_cont + federal_igrants_cont,
           federal_cont = federal_nom_cont + federal_grants_cont,
           state_local_cont_ex_grants = state_local_nom_cont,
           state_local_cont = state_local_nom_cont - federal_grants_cont,
           purchases_cont = federal_cont + state_local_cont)
}

fim <-
  fim %>%
  contributions_purchases_grants() %>%
  total_purchases()
# 4.4 Counterfactual Taxes and Transfers -------------------------------------------------------------------------------
remove_social_benefit_components <- function(df, component){
  remove_unemployment_insurance <- function(df){
    df %>%
      mutate(social_benefits = social_benefits - unemployment_insurance,
             federal_social_benefits = federal_social_benefits - federal_unemployment_insurance,
             state_social_benefits = state_social_benefits - state_unemployment_insurance)
  }
  remove_rebate_checks <- function(df){
    df %>%
      mutate(social_benefits = social_benefits - rebate_checks,
             federal_social_benefits = federal_social_benefits - federal_rebate_checks,
             state_social_benefits = state_social_benefits - state_rebate_checks)
  }
  df %>%
    remove_unemployment_insurance() %>%
    remove_rebate_checks()
}
neutral <- function(x){
  lag(x) * (1 + fim$gdppoth + fim$pi_pce)
}
taxes_transfers_minus_neutral <- function(df){
  taxes_transfers <- c("subsidies","health_outlays", "social_benefits",
                       "noncorp_taxes", "corporate_taxes", 'rebate_checks', 
                       'unemployment_insurance')
  government_level <- c('federal', 'state')
  all_taxes_transfers <- c(glue('{taxes_transfers}'), glue('federal_{taxes_transfers}'),
                           glue('state_{taxes_transfers}'))
  df %>%
    mutate(
      across(.cols = all_of(all_taxes_transfers),
             .fns = ~ . - neutral(.),
             .names = '{.col}_minus_neutral')
    )
  
}
calculate_health_mpc <- function(df){
  taxes_transfers <- 'health_outlays'
  net <- 'minus_neutral'
  government_levels <- c(glue('{taxes_transfers}_{net}'), glue('federal_{taxes_transfers}_{net}'),
                         glue('state_{taxes_transfers}_{net}'))
  total <- glue('{taxes_transfers}_post_mpc')
  federal <- glue('federal_{taxes_transfers}_post_mpc')
  state <- glue('state_{taxes_transfers}_post_mpc')
  
  df %>%
    mutate(
      across(
        .cols = all_of(government_levels),
        .fns = ~ mpc_health_outlays(.x),
        .names = '{.col}_xmpc'
      )
    ) %>%
    rename(!!total := glue('{taxes_transfers}_{net}_xmpc'),
           !!federal := glue('federal_{taxes_transfers}_{net}_xmpc'),
           !!state := glue('state_{taxes_transfers}_{net}_xmpc'))
}
calculate_social_benefits_mpc <- function(df){
  taxes_transfers <- 'social_benefits'
  net <- 'minus_neutral'
  government_levels <- c(glue('{taxes_transfers}_{net}'), glue('federal_{taxes_transfers}_{net}'),
                         glue('state_{taxes_transfers}_{net}'))
  total <- glue('{taxes_transfers}_post_mpc')
  federal <- glue('federal_{taxes_transfers}_post_mpc')
  state <- glue('state_{taxes_transfers}_post_mpc')
  
  df %>%
    mutate(
      across(
        .cols = all_of(government_levels),
        .fns = ~ mpc_social_benefits(.x),
        .names = '{.col}_xmpc'
      )
    ) %>%
    rename(!!total := glue('{taxes_transfers}_{net}_xmpc'),
           !!federal := glue('federal_{taxes_transfers}_{net}_xmpc'),
           !!state := glue('state_{taxes_transfers}_{net}_xmpc'))
}
calculate_mpc <- function(df, taxes_transfers){

  net <- 'minus_neutral'
  government_levels <- c(glue('{taxes_transfers}_{net}'), glue('federal_{taxes_transfers}_{net}'),
                         glue('state_{taxes_transfers}_{net}'))
  total <- glue('{taxes_transfers}_post_mpc')
  federal <- glue('federal_{taxes_transfers}_post_mpc')
  state <- glue('state_{taxes_transfers}_post_mpc')
  
  if(taxes_transfers == 'subsidies'){
    second_draw <- as_date('2021-03-31')
    mpc_fun <- eval(sym(glue('mpc_{taxes_transfers}')))
    mpc_fun_second_draw <- eval(sym(glue('mpc_{taxes_transfers}_second_draw')))
    df %>%
      mutate(
        across(
          .cols = all_of(government_levels),
          .fns = ~ if_else(date < second_draw, 
                           mpc_fun(.),
                           mpc_fun_second_draw(.)),
          .names = '{.col}_xmpc'
        )
      ) %>%
      rename(!!total := glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue('state_{taxes_transfers}_{net}_xmpc'))
  }
  else{
    mpc_fun <- eval(sym(glue('mpc_{taxes_transfers}')))
    df %>%
    mutate(
      across(
        .cols = all_of(government_levels),
        .fns = ~ mpc_fun(.),
        .names = '{.col}_xmpc'
      )
    ) %>%
    rename(!!total := glue('{taxes_transfers}_{net}_xmpc'),
           !!federal := glue('federal_{taxes_transfers}_{net}_xmpc'),
           !!state := glue('state_{taxes_transfers}_{net}_xmpc'))
  }
}
fim <-
  fim %>%
  remove_social_benefit_components() %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc('subsidies') %>%
  calculate_mpc('health_outlays') %>%
  calculate_mpc('social_benefits') %>%
  calculate_mpc('unemployment_insurance') %>%
  calculate_mpc('rebate_checks') %>%
  calculate_mpc('noncorp_taxes') %>%
  calculate_mpc('corporate_taxes') 

# 4.5 Taxes and Transfers ----------------------------------------------------------------------------------------------
add_social_benefit_components <- function(df){
  add_unemployment_insurance <- function(df){
    df %>%
      mutate(social_benefits = social_benefits + unemployment_insurance,
             federal_social_benefits = federal_social_benefits + federal_unemployment_insurance,
             state_social_benefits = state_social_benefits + state_unemployment_insurance)
  }

  add_rebate_checks <-function(df){
    df %>%
      mutate(social_benefits = social_benefits + rebate_checks,
             federal_social_benefits = federal_social_benefits + federal_rebate_checks,
             state_social_benefits = state_social_benefits + state_rebate_checks)
  }

  df %>%
    add_unemployment_insurance() %>%
    add_rebate_checks()
}
taxes_contributions <- function(df){
  taxes <- c('noncorp_taxes_post_mpc', 'corporate_taxes_post_mpc')
  all_taxes <- c(glue('{taxes}'), glue('federal_{taxes}'), glue('state_{taxes}')) 
  df %>%
    mutate(
      across(
        .cols  = all_of(all_taxes),
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_cont"
      )
    ) %>%
  rename_with(~ gsub('post_mpc_cont', 'cont', .x))
    
}
sum_taxes_contributions <- function(df){
  taxes <- c('noncorp_taxes', 'corporate_taxes')
  df %>%
    mutate(
      taxes_cont = rowSums(select(., .dots = all_of(str_glue('{taxes}_cont')))),
      federal_taxes_cont = rowSums(select(., .dots = all_of(str_glue('federal_{taxes}_cont')))),
      state_taxes_cont = rowSums(select(., .dots = all_of(str_glue('state_{taxes}_cont'))))
    )
}
transfers_contributions <- function(df){
  transfers <- c('social_benefits', 'health_outlays', 'subsidies',
                 'unemployment_insurance', 'rebate_checks') %>%
    paste0('_post_mpc')
  all_transfers <- c(glue('{transfers}'), glue('federal_{transfers}'), glue('state_{transfers}')) 
  df %>%
    mutate(
      across(
        .cols  = all_of(all_transfers),
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_cont"
      )
    ) %>%
    rename_with(~ gsub('post_mpc_cont', 'cont', .x))
}
sum_transfers_contributions <- function(df){
  transfers <- c('social_benefits',  'health_outlays', 'subsidies',
                 'unemployment_insurance', 'rebate_checks')
  df %>%
    mutate(
       transfers_cont = rowSums(select(., .dots = all_of(str_glue('{transfers}_cont'))), na.rm = TRUE),
       federal_transfers_cont = rowSums(select(., .dots = all_of(str_glue('federal_{transfers}_cont'))), na.rm = TRUE),
       state_transfers_cont = rowSums(select(., .dots = all_of(str_glue('state_{transfers}_cont'))), na.rm = TRUE)
      )
}

sum_taxes_transfers <- function(df){
  df %>%
    mutate(
      taxes_transfers_cont = taxes_cont + transfers_cont,
      federal_taxes_transfers_cont = federal_taxes_cont + federal_transfers_cont,
      state_taxes_transfers_cont = state_taxes_cont + state_transfers_cont,
    )
}
get_fiscal_impact <- function(df){
  df %>%
    mutate(fiscal_impact = federal_cont + state_local_cont + taxes_transfers_cont,
           fim_bars = fiscal_impact,
           fiscal_impact_moving_average = SMA(na.locf(fiscal_impact, na.rm = F), n=4),
           fim_bars_ma = fiscal_impact_moving_average
    )
}

skim_contributions <- function(df){
  df %>%
     select(date, ends_with('cont')) %>% 
    filter(date > '2019-12-31') %>%
    skim()
}
get_transfers <- function(){
  fim %>%
  taxes_contributions() %>%
  sum_taxes_contributions() %>%
  transfers_contributions() %>%
  sum_transfers_contributions() %>%
  sum_taxes_transfers() %>% 
  select(date, transfers_cont, social_benefits_cont, health_outlays_cont,
         subsidies_cont, unemployment_insurance_cont, rebate_checks_cont,
         federal_transfers_cont, federal_social_benefits_cont,
         federal_health_outlays_cont, federal_subsidies_cont,
         federal_unemployment_insurance_cont, federal_rebate_checks_cont,
         state_transfers_cont, state_social_benefits_cont,
         state_health_outlays_cont, state_subsidies_cont,
         state_unemployment_insurance_cont, state_rebate_checks_cont) %>% 
  mutate(sum = federal_transfers_cont + state_transfers_cont,
         diff = transfers_cont - sum) %>% filter(date > '2019-12-31') 
}
get_taxes <- function(){
  fim %>%
    taxes_contributions() %>%
    sum_taxes_contributions() %>%
    filter(date > '2019-12-31') %>%
    select(date, taxes_cont, corporate_taxes_cont, 
           noncorp_taxes_cont, federal_taxes_cont, 
           federal_corporate_taxes_cont, 
           federal_noncorp_taxes_cont,
           state_taxes_cont, state_corporate_taxes_cont,
           state_noncorp_taxes_cont) %>%
    mutate(total = taxes_cont, 
           sum = federal_taxes_cont + state_taxes_cont,
           diff = total - sum)
}
  
transfers <- get_transfers
fim <-
  fim %>%
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


# 4.6 Export data -------------------------------------------------------------------------------------------

# 4.6.2 Website interactive ----------------------------------------------------------------------------------
prepare_interactive <- function(df){
  firstDate <- "1999-12-31"
  df %>% 
    filter(date >= firstDate) %>% 
    mutate(
           yrq = as.yearqtr(date),
           projection = if_else(date > last_hist_date,
                                1,
                                0),
           taxes_transfers_subsidies_cont = taxes_transfers_cont
           ) %>%
    separate(yrq, c("year", "quarter")) %>%
    select(year, quarter, fim_bars_ma, recession,
           fim_bars, 
           federal_cont, state_local_cont,
           taxes_transfers_subsidies_cont,
           projection) %>%
    rename(
      "total" = fim_bars,
      "impact" = fim_bars_ma,
      "federal" = federal_cont,
      "state_local" = state_local_cont,
      "consumption" = taxes_transfers_subsidies_cont
    ) %>% 
  mutate(recession = if_else(is.na(recession),
                             0,
                             recession))
}
fim_interactive <-
  fim %>%
  prepare_interactive()
# 4.6.3 Create CSV files --------------------------------------------------------------------------------
export_results <- function(){
# Create folder for current month's update
current_month <- glue('{month(today())}-{year(today())}')
dir.create(here('results', current_month))

# Write csv to current month's folder
results <- 
  list(fim = fim,
                fim_interactive = fim_interactive,
                projections = projections)

output_xlsx <- function(data, names){ 
  write_xlsx(data, glue('results/{current_month}/{names}.xlsx'))
}

list(data = results, 
     names = names(results)) %>%
  purrr::pmap(output_xlsx) 
}
export_results()


