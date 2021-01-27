
# Constants -----------------------------------------------------------------------------------

last_hist_date <-
  hist %>%
  pull(date) %>%
  max()

last_proj_date <- last_hist_date + years(2)

### 2.1.1 CBO Projections --------------------------------------------------------------------------------------

budg <-
  budg %>% 
  annual_to_quarter(fy)

cbo_projections <-
  budg %>% 
  left_join(econ)

cbo_projections_calculations <- function(){
cola_adjustment <- function() {
  cbo_projections %>%
    mutate(cpiu_g = q_a(cpiu) / 100,
           cola_rate = if_else(month(date) == 3,
                               lag(cpiu_g, 2),
                               NULL) 
    ) %>%
    fill(cola_rate) %>%
    mutate(
      gftfp_before_cola = gftfp,
      health_ui = SMA(yptmd + yptmr + yptu, n = 4),
      gftfp_noCOLA = SMA((gftfp - health_ui)*(1-cola_rate), n = 4),
      gftfp =  gftfp_noCOLA * (1 + cola_rate)  + health_ui) 
} 
smooth_budget_series <- function(df){
  # smooth all budget series except total social transfers, which we did above
  df %>%
  mutate(
    across(.cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd"),
           .fns = ~ rollapply(.x, width = 4, mean, fill = NA, align =  'right')
    ) 
  )
}
alternative_tax_scenario <- function(df){
  # Construct alternative scenario for personal current taxes, under which the TCJA provisions for income taxes don't
  # expire in 2025
  expdate <- "2025-12-30"
  predate <- "2025-09-30"
  
  df %>%
    mutate(gfrptCurrentLaw = gfrpt,
           gfrptCurrentLaw_g = gfrpt_g,
           gfrpt_g =
             if_else(date >= expdate,
                     lag(gfrpt_g),
                     gfrpt_g,
                     missing = NULL
             ),
           gfrpt  = if_else(date >= predate,
                            lag(gfrpt) * (1 + gfrpt_g / 400),
                            gfrpt))
}
implicit_price_deflators <- function(df){
  # Implicit price deflators
  df %>% 
    mutate(jgf =  gf/gfh,
           jgs = gs/gsh,
           jc = c/ch) 
}
state_taxes <- function(df){
    df %>% 
      left_join(hist %>%
                  select(date, gsrpt ,gsrpri, gsrcp ,gsrs),
                all.x = F) %>%
      filter(date > '2016-12-31') %>%
      mutate(
        across(
          .cols = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs"),
          .fns = ~ na.locf(. / gdp) * gdp
        )
      )
}
growth_rates <- function(df){
  df %>%
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ q_g(.),
        .names = "{.col}_g"
      ) 
    )
}
cola_adjustment() %>%
smooth_budget_series() %>%
  mutate(
    across(
      .cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd" ),
      .fns = ~ q_g(.x),
      .names = "{.col}_g"
    )
  ) %>%
  alternative_tax_scenario() %>%
  implicit_price_deflators() %>% 
  state_taxes() %>%
  growth_rates()
}
cbo_projections <- cbo_projections_calculations()



## 4 Merge growth rates and levels data frames ---------------------------------------------------------------------
projections <-
    full_join(hist,
              cbo_projections %>%
                select(date, contains('_g')),
              by = 'date')


### 4.2.1 ------------------------------------------------------------------------------------------
unemployment_insurance_override <-
  read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
           sheet = "FIM Add Factors") %>%
  mutate(date = as_date(date)) %>%
  select(date, contains('unemployment_insurance')) 
  
projections %<>%
  left_join(unemployment_insurance_override) %>%
  mutate(
    across(
      .cols = ends_with('override'),
      .fns = ~ if_else(is.na(.x), 0, .x)
    )
  )
  
# Federal UI legislation total from Q3 of 2020 is $768.8 (Bil. $)
millions_to_billions <- function(df){
  df %>% 
    mutate(
      across(.cols = c('gftfbusx', 'gfeghhx', 'gfeghdx', 'gfeigx'),
             .fns = ~ . / 1000)
    )
}
unemployment_insurance_reallocation <- function(df){
  df %>%
    mutate(
      gftfp = gftfp - yptu + federal_unemployment_insurance_override,
      gstfp = gstfp + yptu - federal_unemployment_insurance_override
    )
}
fmap_share <- function(df){
 df %>%
   left_join(fmap %>% filter(year >= 1970) %>% annual_to_quarter(year) %>% select(date, fshare) %>% na.locf())
}
fmap_share_old <- function(df){
  df %>%
    mutate(
      fshare = fmap$fshare[match(year(date), fmap$year)] %>%
                   na.locf())
}

projections <-
  projections %>%
    millions_to_billions() %>%
    unemployment_insurance_reallocation() %>%
    fmap_share_old()
# Reuse comments ------------------------------------------------------------------------------




# 4.2.3 Growth Rates Assumptions -----------------------------------------------------------------------------------------
subsidies_growth <- function(df){
  df %>% 
    mutate(
      gfsub_g = gdppothq_g,
      gssub_g = gdppothq_g
    )
}
purchases_growth <- function(df){
  capExpiration <- "2021-09-30"
  df %>%
    mutate(gf_g = if_else(date > capExpiration,
                          gdppothq_g + jgdp_g,
                          gf_g)
    )
}
transfers_growth <- function(df){
  df %>%
    mutate(
      gftfpnet_g = gftfp_g, 
      gstfp_g = gs_g,
      gstfpnet_g =  gs_g,
    )
}
health_growth <- function(df){
  df %>%
    mutate(
      #  Health & Hospital grants to states growth with Medicaid
      gfeghhx_g = yptmd_g, 
      # Medicaid grants to states grow with medicaid
      gfeghdx_g = yptmd_g, 
    )
}
grants_growth <- function(df){
  df %>%
    mutate(
      # Aid to S&L grow with federal purchases
      gfeg_g = gf_g, 
      # Capital grants to state and local gov'ts grow with federal purchases
      gfeigx_g = gf_g
    )
}
deflators_growth <- function(df){
  df %>%
    mutate(
      jgsi_g = jgs_g,
      # Consumption deflator grows with overall deflator for S&L
      jgse_g = jgs_g
    )
}
growth_assumptions <- function(df){
  df %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth()
    
}
state_purchases_growth_override <- function(df){
#   projections %>% filter(date < '2020-12-31') %>% count(n()) %>% pull()
#   rate <- c(rep(0.0025,3), 0.005,0.0075,0.01)
#   projections %>% mutate(replacement_rate = if_else(date >= '2020-12-31' & date <= '2022-03-31',
#   )
}


projections <-
  projections %>%
  growth_assumptions()
## Louise override CBO growth rate for S&L purchases for Q42020 through (& including) Q12022
medicaid_reallocation <- function(df){
  df %>%
    mutate(
      # Reattribute federal grants to states back to Federal government
      # Parse between those for consumption and investment and those for transfers (Medicaid)
      
      # federal medicaid grants to states
      yfptmd = if_else(date >='1993-03-31',
                       gfeghdx,
                       yptmd*fshare),
      
      
      # state medicaid payments = total medicaid - federal medicaid grants
      ysptmd = yptmd - yfptmd,
      gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
      
      # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
      gstfpnet = gstfp - yfptmd, 
      # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
      gftfpnet = gftfp + yfptmd 
      # we reattribute the capital grants later after calculating contributions. 
    )
}



# 5 Forecast ----------------------------------------------------------------------------------------------------

## Generate forecastPeriod values of components using current levels and our projected growth rates. 


forecast_period <- function(){
  forecastPeriod <- which(projections$date > last_hist_date)
  return(forecastPeriod)
}
components <- function(){
  components <-
    c(
      # GDP
      ## Actual
      'gdp', 'gdph', 'jgdp',
      ## Potential
      'gdppotq', 'gdppothq',
      # Gov't Consumption Expenditures & Gross Investment 
      ## Total 
      'g', 'gf', 'gs', 
      ## Deflators
      ### Total
      'jgf', 'jgs',
      ### S&L Consumption/Investment Expenditures
      'jgse', 'jgsi',
      # GRANTS
      ## Total
      'gfeg', 
      ### Health & Hospitals
      'gfeghhx',
      ### Medicaid
      'gfeghdx', 
      ### Investment
      'gfeigx', 
      # TAXES
      ## Personal
      'gfrpt', 'gsrpt',
      ## Social Insurance
      'gfrs' ,'gsrs', 
      ## Corporate
      'gfrcp', 'gsrcp',
      ## Production & Imports
      'gfrpri', 'gsrpri',
      # SOCIAL BENEFITS
      ## Total
      'gftfp', 'gstfp',
      ## Medicaid
      'yptmd',
      ## Medicare
      'yptmr',
      # SUBSIDIES 
      'gssub', 'gfsub', 
      # PERSONAL CONSUMPTION
      'c', 'jc'
    )
  return(components)
}

for(f in forecast_period()){
  projections[f,components()] = projections[f-1, components()]  * (1 + projections[f, paste0(components(), "_g")])
}

aggregate <- function(df, total, federal, state){
  df %>%
    mutate({{total}} := if_else(date > last_hist_date, {{federal}} + {{state}}, {{total}})
    )
}
total_forecast <- function(df){
  df %>%
    aggregate(gtfp, gftfp, gstfp) %>%
    aggregate(yptx, gfrpt, gsrpt) %>%
    aggregate(ytpi, gfrpri, gsrpri) %>%
    aggregate(grcsi, gfrs, gsrs) %>%
    aggregate(grcsi, gfrs, gsrs) %>%
    aggregate(yctlg, gfrcp, gsrcp) %>%
    aggregate(gsub, gfsub, gssub)
}

projections <-
  projections %>%
  total_forecast() %>%
  medicaid_reallocation()





