#library(targets)
#library(tarchetypes)
source('R/packages.R')
source('R/functions.R')
library('conflicted')
conflicted::conflict_prefer('filter', 'dplyr')
conflict_prefer("month", "lubridate")
conflict_prefer('year', 'lubridate')
conflict_prefer('lag', 'dplyr')

current_month <- glue::glue(lubridate::month(lubridate::today()), '-', lubridate::year(lubridate::today()))
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
#tar_option_set(packages = "dplyr")

load_unemployment_insurance_override <- function (){
  readxl::read_excel(drake::file_in("data/add-ons/add_factors.xlsx"),
                     sheet = "FIM Add Factors") %>% dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::select(date, tidyselect::contains("unemployment_insurance"))
}

override_projections <-function (df) {
  override <- readxl::read_excel("data/add-ons/add_factors.xlsx", 
                                 sheet = "FIM Add Factors") %>% dplyr::select(date, 
                                                                              ends_with("override")) %>% mutate(date = lubridate::as_date(date))
  Q2_2020 <- "2020-06-30"
  Q3_2020 <- "2020-09-30"
  Q1_2021 <- "2021-03-31"
  last_override <- "2022-12-31"
  df %>% left_join(override, by = "date") %>% mutate(unemployment_insurance = if_else(date >= 
                                                                                        Q3_2020 & date <= last_override, unemployment_insurance_override, 
                                                                                      unemployment_insurance), federal_unemployment_insurance = if_else(date >= 
                                                                                                                                                          Q2_2020 & date <= last_override, federal_unemployment_insurance_override, 
                                                                                                                                                        federal_unemployment_insurance), state_unemployment_insurance = if_else(date >= 
                                                                                                                                                                                                                                  Q2_2020 & date <= last_override, state_unemployment_insurance_override, 
                                                                                                                                                                                                                                state_unemployment_insurance), federal_cgrants = if_else(date >= 
                                                                                                                                                                                                                                                                                           Q2_2020 & date <= Q1_2021, federal_cgrants_override, 
                                                                                                                                                                                                                                                                                         federal_cgrants))
}


add_factors <-
  function(df) {
    add_factors <- readxl::read_excel("data/add-ons/add_factors.xlsx",
                                      sheet = "FIM Add Factors") %>% mutate(date = lubridate::as_date(date))
    df %>% dplyr::full_join(
      add_factors %>% dplyr::select(-tidyselect::ends_with("override")) %>%
        filter(date > '2021-03-31'),
      by = "date"
    ) %>% dplyr::mutate(dplyr::across(
      .cols = tidyselect::starts_with("add_"),
      .fns = ~
        if_else(is.na(.x), 0, .x)
    )) %>% dplyr::mutate(
      state_health_outlays = state_health_outlays +
        add_state_health_outlays,
      state_social_benefits = state_social_benefits +
        add_state_social_benefits,
      federal_health_outlays = federal_health_outlays +
        add_federal_health_outlays,
      federal_social_benefits = federal_social_benefits +
        add_federal_social_benefits,
      federal_subsidies = federal_subsidies +
        add_federal_subsidies,
      federal_cgrants = federal_cgrants +
        add_federal_cgrants,
      state_local_nom = state_local_nom +
        add_state_purchases,
      federal_nom = add_federal_purchases +
        federal_nom,
      health_outlays = state_health_outlays +
        federal_health_outlays,
      social_benefits = state_social_benefits +
        federal_social_benefits,
      subsidies = state_subsidies +
        federal_subsidies,
      federal_rebate_checks = federal_rebate_checks +
        add_rebate_checks,
      rebate_checks = rebate_checks + add_rebate_checks
    )
  }


# End this file with a list of target objects.

cbo_projections_raw = load_cbo_projections()
fmap = read_xlsx(here('data/raw/nhe_fmap.xlsx'))
historical = load_national_accounts() %>% mutate(date = as_date(date))
last_hist_date = get_last_hist_date(historical)
last_proj_date = last_hist_date + lubridate::years(2)
cbo_projections = load_cbo_projections() %>%
  cola_adjustment() %>% 
  smooth_budget_series() %>%
  federal_transfers_growth_rates() %>%
  health_outlays_growth_rates() %>%
  alternative_tax_scenario() %>%
  implicit_price_deflators() %>%
  state_taxes() %>%
  growth_rates()
unemployment_insurance_override = load_unemployment_insurance_override()
components = get_components_names()
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
#comma here? if breaks
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
    start = '2021-06-30',
    end = '2023-06-30',
   values = c(0.00625, rep(0.0075, 3), 0.00625, 0.005, rep(0.0025, 3)) )  %>%
  components_growth_rates() %>%
  mutate(gftfp = gftfp - federal_unemployment_insurance_override - gftfpe, gtfp = gstfp + gftfp) %>%
  create_projections() %>%
  medicaid_reallocation() 

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
  total_purchases() %>%
  # remove_social_benefit_components() %>%
  
  mutate(rebate_checks = if_else(date == '2021-03-31', rebate_checks - 1348, rebate_checks), rebate_checks_arp = if_else(date == '2021-03-31', 1348, rebate_checks_arp), federal_subsidies = if_else(date == '2021-03-31', 753, federal_subsidies), subsidies = federal_subsidies + state_subsidies) %>% 
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
 #calculate_mpc('rebate_checks') %>%
  mutate(rebate_checks_post_mpc= fim::mpc_rebate_checks(rebate_checks_minus_neutral), federal_rebate_checks_post_mpc = fim::mpc_rebate_checks(federal_rebate_checks_minus_neutral), state_rebate_checks_post_mpc = 0) %>% 
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
  mutate(
    fiscal_impact = federal_cont + state_local_cont + taxes_cont + state_transfers_cont +  federal_transfers_cont_no_arp + federal_ui_arp_cont + state_ui_arp_cont + rebate_checks_arp_cont + aid_to_small_businesses_cont + health_grants_arp_cont + other_direct_aid_cont + other_vulnerable_cont,
    arp_cont =  health_grants_arp_cont + non_health_grants_cont +
      federal_ui_arp_cont + rebate_checks_arp_cont + other_direct_aid_cont + other_vulnerable_cont + aid_to_small_businesses_cont
  ) %>%
  mutate(
    federal_unemployment_insurance = federal_unemployment_insurance + federal_ui_arp,
    federal_unemployment_insurance_cont = federal_unemployment_insurance_cont + federal_ui_arp_cont,
    state_unemployment_insurance = state_unemployment_insurance + state_ui_arp,
    state_unemployment_insurance_cont = state_unemployment_insurance_cont + state_ui_arp_cont,
    federal_purchases_with_grants = federal_nom + federal_cgrants + federal_igrants,
    state_purchases_with_grants = state_local_nom - federal_cgrants - federal_igrants
  ) %>%
  
  
  arrange(date, recession, fiscal_impact, fiscal_impact_moving_average,
          federal_cont, state_local_cont,
          taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont)

fim <- fim %>% mutate(rebate_checks = rebate_checks + rebate_checks_arp) %>% mutate(rebate_checks_cont = rebate_checks_cont + rebate_checks_arp_cont)


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

fim %>% prepare_interactive() %>% write_xlsx('results/4-2021/fim-interactive-2021-04.xlsx')




