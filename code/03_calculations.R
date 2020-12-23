
# 5 Construct FIM data frame for calculations -----------------------------------------------------------
fim <-
  xx %>%
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

# 5.2 Add-ons  ------------------------------------------------------------------------------------------

# Created to adjust our forecasts for the coronavirus bills
# Remove when NIPAS are updated or new economic projections are released (whichever comes first)

#load add factor file
add_factors <- read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                          sheet = "FIM Add Factors") %>%
                  mutate(
                    date = as.Date(date)
                  ) 
 
fim <-
  fim %>% 
  full_join(add_factors %>% select(-ends_with('override')) %>%
              filter(date > last_hist_date),
            by = "date") %>%
  mutate(across(
    .cols = starts_with('add_'),
    .fns = ~ if_else(is.na(.x), 
                     0,
                     .x)
    )
  )
# New Totals
fim <- 
  fim %>%
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


# Overrides -----------------------------------------------------------------------------------
# Load add factor file and select override columns
override <- read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                          sheet = "FIM Add Factors") %>%
  select(date, ends_with('override')) %>%
  mutate(date = as_date(date)) 

Q2_2020 <- '2020-06-30'
Q3_2020 <- '2020-09-30'
last_override <- '2022-12-31'
fim <-
  fim %>%
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

  

# 4.3 Contribution of purchases and grants -------------------------------------------------------------------------------------

## Calculate contributions

fim <-
  map(
    alist(federal_nom, state_local_nom, federal_cgrants, federal_igrants),
    ~ contribution(fim, !!.x)
  ) %>%
  reduce(left_join) %>%
  left_join(fim, .)

# Sum up purchases, taking out the federal grants contribution from state and local and adding it back to federal. 
fim <-
  fim %>%
    mutate(federal_cont_ex_grants = federal_nom_cont,
           federal_grants_cont = federal_cgrants_cont + federal_igrants_cont,
           federal_cont = federal_nom_cont + federal_grants_cont,
           state_local_cont_ex_grants = state_local_nom_cont,
           state_local_cont = state_local_nom_cont - federal_grants_cont,
           purchases_cont = federal_cont + state_local_cont)
  
# 4.4 Counterfactual Taxes and Transfers -------------------------------------------------------------------------------

fim %<>%
  mutate(social_benefits = social_benefits - federal_rebate_checks - unemployment_insurance,
         federal_social_benefits = federal_social_benefits - federal_rebate_checks - federal_unemployment_insurance,
         state_social_benefits = state_social_benefits - state_unemployment_insurance)
## Category totals
tt = c("subsidies","health_outlays", "social_benefits", "noncorp_taxes", "corporate_taxes",
       'rebate_checks', 'unemployment_insurance')

## Category totals by level of government
tts = c(tt, paste0("federal_", tt), paste0("state_", tt)) # totals and disaggregations by level of goverment


# Calculate "net" taxes and transfers by subtracting counterfactual from realized.
# We specify the counterfactual as the lag times the growth rate of potential gdp and our deflator
fim <-  
  fim %>%
    mutate(
      across(
        .cols = all_of(tts), 
        .fns =  ~ . - lag(.) * (1 + pi_pce + gdppoth),
        .names = "{.col}_net"
      )
    ) %>%
    fill(ends_with("_net"))

tts <- paste0(tts, "_net") # rename for efficiency
tt <- paste0(tt, "_net") # rename for efficiency

# 4.5 MPCs ----------------------------------------------------------------------------------------------

#######

# Take tax and transfer category totals, net of counterfactual taxes, multiply by MPC's
health = grep("health", tts, value=T)
social_benefits = grep("social_benefits", tts, value=T)
noncorp = grep("noncorp", tts, value=T)
corporate = grep("corporate", tts, value=T)
subsidies = grep("subsidies", tts, value=T)

# Translate Taxes & Transfers into Consumption --------------------------------------------------------------------
covid_start <- as.Date('2020-06-30')

unemployment_insurance <- paste0(c('unemployment_insurance', 'state_unemployment_insurance',
                                  'federal_unemployment_insurance'),
                                '_net')

rebate_checks <-
  paste0(c('rebate_checks', 'state_rebate_checks', 'federal_rebate_checks'), '_net')

## Dates for new MPCs

nlag <- 12
mpc_lag <-
  fim %>%
  select(date) %>%
  slice(
    which(date == covid_start) - (nlag - 1)
  )


## CALCULATE MPCS
covid_end <- as.Date('2025-12-31')
round2 <- as.Date('2021-03-31')
fim <-
  fim %>% 
    ## HEALTH OUTLAYS
    mutate(
      across(
        .cols = all_of(health),
        .fns = ~ if_else(date >= covid_start & date <= covid_end,
                         mpc_health_outlays_CRN19(.x),
                         mpc_health_outlays(.x)),
        .names = "{.col}_xmpc"
      )
    ) %>%
    ## SOCIAL BENEFITS
    mutate(
      across(
        .cols = all_of(social_benefits),
        .fns = ~ if_else(date >= covid_start & date <= covid_end,
                         mpc_social_benefits(.x),
                         mpc_social_benefits(.x)
        ),
        .names = "{.col}_xmpc"
      )
    ) %>%
  mutate(
    across(
      .cols = all_of(unemployment_insurance),
      .fns = ~ mpc_ui_CRN19(.x),
      .names = "{.col}_xmpc"
    )
  ) %>%
  mutate(
    across(
      .cols = all_of(rebate_checks),
      .fns = ~ mpc_rebate_CRN19(.x),
      .names = "{.col}_xmpc"
    )
  )  %>%
    ## CORPORATE TAXES
    mutate(
      across(
        .cols = all_of(corporate),
        .fns = ~ if_else(date >= covid_start & date <= covid_end,
                         mpc_corporate_taxes_CRN19(.x),
                         mpc_corporate_taxes(.x)
        ),
        .names = "{.col}_xmpc"
      )
    ) %>%
    ## NON-CORPORATE TAXES
    mutate(
      across(
        .cols = all_of(noncorp),
        .fns = ~ if_else(date >= covid_start & date <= covid_end,
                         mpc_noncorp_taxes_CRN19(.x),
                         mpc_noncorp_taxes(.x)
        ),
        .names = "{.col}_xmpc"
      )
    ) %>%
    ## SUBSIDIES
    mutate(
      across(
        .cols = all_of(subsidies),
        .fns = ~ if_else(date >= round2,
                         mpc_ppp_round2(.x),
                         mpc_subsidies(.x)
        ),
        .names = "{.col}_xmpc"
      )
    )

# Add rebate and ui back into social benefits
# 
fim %<>% 
  mutate(social_benefits_net_xmpc = social_benefits_net_xmpc + unemployment_insurance_net_xmpc + rebate_checks_net_xmpc,
         state_social_benefits_net_xmpc = state_social_benefits_net_xmpc + state_unemployment_insurance_net_xmpc + state_rebate_checks_net_xmpc,
         federal_social_benefits_net_xmpc = federal_social_benefits_net_xmpc + federal_unemployment_insurance_net_xmpc + federal_rebate_checks_net_xmpc)

# Sum up transfers net taxes
tt = paste0(c("subsidies","health_outlays", "social_benefits", "noncorp_taxes", "corporate_taxes"),
            '_net')

govt_taxes_transfers <- paste0(tt, "_xmpc")
state_taxes_transfers <- paste0("state_", tt, "_xmpc")
federal_taxes_transfers <- paste0("federal_", tt, "_xmpc")
fim <-
  fim %>%
  mutate(
    transfers_net_taxes = rowSums(
      select(., .dots = all_of(govt_taxes_transfers)), na.rm = TRUE
    ),
    state_transfers_net_taxes = rowSums(
      select(., .dots = all_of(state_taxes_transfers)), na.rm = TRUE
    ),
    federal_transfers_net_taxes = rowSums(
      select(., .dots = all_of(federal_taxes_transfers)), na.rm = TRUE
    )
  )


# Taxes, Transfers, & Subsidies Contributions ---------------------------------------------------------------------

## Calulate the taxes, transfers, and subsidies FIM

tts <-  c("taxes_transfers", "state_taxes_transfers", "federal_taxes_transfers",
          "health", "social_benefits", "noncorp",
          "corporate", "state_subsidies", "federal_subsidies",
          "subsidies")
contributionTTS <- paste0(
  tts,
  '_cont'
)

net <- c("transfers_net_taxes", "state_transfers_net_taxes", "federal_transfers_net_taxes",
         "health_outlays_net_xmpc", "social_benefits_net_xmpc", "noncorp_taxes_net_xmpc",
         "corporate_taxes_net_xmpc", "state_subsidies_net_xmpc", "federal_subsidies_net_xmpc",
         "subsidies_net_xmpc")

fim <-
  fim %>%
    mutate(
      across(
        .cols  = net,
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_cont"
      )
    ) %>%
  setnames(paste0(net, "_cont"), contributionTTS) #%>%
  ## Add contribution of subsidies to contribution of taxes and transfers
  # I think this is double counting
# mutate(
  #   taxes_transfers_cont = taxes_transfers_cont + subsidies_cont,
  #   federal_taxes_transfers_cont = federal_taxes_transfers_cont + federal_subsidies_cont,
  #   state_taxes_transfers_cont = state_taxes_transfers_cont + state_subsidies_cont
  # )

# We forecast two years ahead
####change to 6 quarters ahead for COVID19 as of August 3rd 2020

fim %<>%
  mutate(social_benefits = social_benefits + unemployment_insurance + rebate_checks,
         federal_social_benefits = federal_social_benefits + federal_unemployment_insurance + rebate_checks,
         state_social_benefits = state_social_benefits + state_unemployment_insurance 
  )

# 4.6 Export data -------------------------------------------------------------------------------------------
firstDate <- "1999-12-31"
saveRDS(fim %>% filter(date > firstDate ), 'data/processed/fim.rds')
# 4.6.1 Clean FIM data frame ----------------------------------------------------------------------------
last_proj_date <- as.Date('2022-12-31')
fim <-
  fim %>% 
    mutate(fim_bars = federal_cont + state_local_cont + taxes_transfers_cont,
           fim_bars_ma = SMA(na.locf(fim_bars, na.rm = F), n = 4)) %>% 
    filter(date <= as.Date(last_proj_date)) %>%
    select(date, fim_bars, fim_bars_ma, state_local_cont, federal_cont, taxes_transfers_cont, 
           subsidies_cont, recession, everything())
# 4.6.2 Website interactive ----------------------------------------------------------------------------------
firstDate <- "1999-12-31"

fim_interactive <- 
  fim %>% 
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
# 4.6.3 Create CSV files --------------------------------------------------------------------------------

# Create folder for current month's update
thismonth <- format(Sys.Date(), "%m-%Y")
dir.create(here('results/', thismonth))

# Write csv to current month's folder
results <- 
  list(fim = fim,
                fim_interactive = fim_interactive,
                xx = xx)

list(data = results, 
     names = names(results)) %>%
  purrr::pmap(output_xlsx) 

