
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
         social_benefits_gross = gtfp,
         social_benefits = social_benefits_gross - health_outlays, # Social benefits net health outlays
         personal_taxes = yptx,
         payroll_taxes = grcsi,
         production_taxes = ytpi,
         noncorp_taxes = personal_taxes + production_taxes + payroll_taxes, # alternative
         corporate_taxes = yctlg,
         subsidies  = gsub,
         ## Federal
         federal_medicaid = yfptmd,
         federal_health_outlays = medicare + federal_medicaid,
         federal_unemployment_insurance = gftfbusx,
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
add_factors <- read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v2.xlsx", 
                          sheet = "FIM Add Factors") %>%
                  mutate(
                    date = as.Date(date)
                  ) 
 
fim <-
  fim %>% 
  full_join(add_factors,
            by = "date") %>%
  mutate(across(
    .cols = starts_with('add_'),
    .fns = ~ if_else(is.na(.x), 
                     0,
                     .x)
    )
  )

# Add factors to categories
covidLegislation <- c('federal_health_outlays', 'federal_social_benefits', 'federal_subsidies',
                      'federal_cgrants', 'state_health_outlays', 'state_social_benefits',
                      'state_noncorp_taxes', 'state_corporate_taxes')
fim[ ,covidLegislation] <- fim[ ,covidLegislation] + fim[ ,paste0('add_', covidLegislation)]

# New Totals
fim <- 
  fim %>%
  mutate(
    # new totals
    health_outlays  = state_health_outlays  + federal_health_outlays ,
    social_benefits  = state_social_benefits  + federal_social_benefits ,
    noncorp_taxes  = state_noncorp_taxes  + federal_noncorp_taxes ,
    corporate_taxes  = state_corporate_taxes  + federal_corporate_taxes ,
    subsidies   = state_subsidies + federal_subsidies,
    # state_local_nom = add_state_expenditures + state_local_nom,
    # federal_nom = add_federal_nom + federal_nom,
    federal_cgrants = add_federal_cgrants + federal_cgrants,
    federal_cgrants = if_else(date == Q2_2020,
                              181.51,
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
    mutate(federal_cont_0 = federal_nom_cont,
           federal_cont = federal_nom_cont + federal_cgrants_cont + federal_igrants_cont,
           state_local_cont_0 = state_local_nom_cont,
           state_local_cont = state_local_nom_cont - (federal_cgrants_cont + federal_igrants_cont),
           purchases_cont = federal_cont + state_local_cont)
  
# 4.4 Counterfactual Taxes and Transfers -------------------------------------------------------------------------------

## Category totals
tt = c("subsidies","health_outlays", "social_benefits", "noncorp_taxes", "corporate_taxes")
## Category totals by level of government
tts = c(tt, paste0("federal_", tt), paste0("state_", tt)) # totals and disaggregations by level of goverment


# Calculate "net" taxes and transfers by subtracting counterfactual from realized 
fim <-  
  fim %>%
    mutate(
      across(
        .cols = tts, 
        .fns =  ~ .x - lag(.x) * (1 + pi_pce + gdppoth),
        .names = "{.col}_net"
      )
    ) %>%
    fill(ends_with("_net"))

tts <- paste0(tts, "_net") # rename for efficiency
tt <- paste0(tt, "_net") # rename for efficiency

# 4.5 MPCs ----------------------------------------------------------------------------------------------

### 4.5.1 Pre-COVID -----------------------------------------------------------------------------------------

mpc_health_outlays = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}

mpc_social_benefits = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}

mpc_noncorp_taxes = function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      lagstart = i-7
      lagend = i-2
      lags = lagstart:lagend
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) 
    }
  }
  j
}

mpc_corporate_taxes = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = -0.4*mean(x[lags]) # distributes out to the MPC applied evenly over 12 quarters
    }
  }
  j
}

mpc_subsidies = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = 0.45*(0.11*x[i]+0.095*x[i-1]+0.09*x[i-2]+0.085*x[i-3]+0.08*x[i-4]+0.08*x[i-5]+0.08*x[i-6]+0.08*x[i-7]+0.075*x[i-8]+0.075*x[i-9]+0.075*x[i-10]+0.075*x[i-11]) 
    }
  }
  j
}


#######

# Take tax and transfer category totals, net of counterfactual taxes, multiply by MPC's
health = grep("health", tts, value=T)
social_benefits = grep("social_benefits", tts, value=T)
noncorp = grep("noncorp", tts, value=T)
corporate = grep("corporate", tts, value=T)
subsidies = grep("subsidies", tts, value=T)

### 4.5.2 Post-COVID ---------------------------------------------------------------------------------------------

#Same as pre-covid
mpc_noncorp_taxes_CRN19 =  function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      lagstart = i-7
      lagend = i-2
      lags = lagstart:lagend
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) 
    }
  }
  j
}


#Same as pre-covid
mpc_health_outlays_CRN19 = function(x){
   0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}

#FIX
mpc_social_benefits_CRN19 = function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      j[i] = 0.86*(0.2879*x[i]+0.2498*x[i-1]+0.19*x[i-2]+0.19*x[i-3] + 0.0253*x[i-4] + 0.0253*x[i-5] + 0.0159*x[i-6] + 0.0159*x[i-7]) # distributes out to 40 percent of the -0.6 MPC applied in first two quarters and the remainder evenly over last 5
    }
  }
  j
}

#Same as pre-covid
mpc_noncorp_taxes_CRN19 =  function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      lagstart = i-7
      lagend = i-2
      lags = lagstart:lagend
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) 
    }
  }
  j
}

#Same as pre-covid
mpc_corporate_taxes_CRN19 = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = -0.40*mean(x[lags]) 
    }
  }
  j
}

# Translate Taxes & Transfers into Consumption --------------------------------------------------------------------

## Dates for new MPCs
mpc_lag <- fim %>% 
  select(date) %>%
  slice(
    which(date == last_hist_date) - 11
  )

nlag <- 12
mpc_lag <-
  fim %>%
  select(date) %>%
  slice(
    which(date == last_hist_date) - (nlag - 1)
  )
Q4_2021 <- "2021-12-31"

## CALCULATE MPCS
fim <-
  fim %>% 
    ## HEALTH OUTLAYS
    mutate(
      across(
        .cols = all_of(health),
        .fns = ~ if_else(date >= mpc_lag & date <= Q4_2021,
                         mpc_health_outlays_CRN19(.x),
                         mpc_health_outlays(.x)),
        .names = "{.col}_xmpc"
      )
    ) %>%
    ## SOCIAL BENEFITS
    mutate(
      across(
        .cols = all_of(social_benefits),
        .fns = ~ if_else(date >= mpc_lag & date <= Q4_2021,
                         mpc_social_benefits_CRN19(.x),
                         mpc_social_benefits(.x)
        ),
        .names = "{.col}_xmpc"
      )
    ) %>%
    ## CORPORATE TAXES
    mutate(
      across(
        .cols = all_of(corporate),
        .fns = ~ if_else(date >= mpc_lag & date <= Q4_2021,
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
        .fns = ~ if_else(date >= mpc_lag & date <= Q4_2021,
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
        .fns = ~ if_else(date >= mpc_lag & date <= Q4_2021,
                         mpc_subsidies(.x),
                         mpc_subsidies(.x)
        ),
        .names = "{.col}_xmpc"
      )
    )


# Sum up transfers net taxes

fim <-
  fim %>%
  mutate(
    transfers_net_taxes = rowSums(
      select(., paste0(tt, "_xmpc")), na.rm = TRUE
    ),
    state_transfers_net_taxes = rowSums(
      select(., paste0("state_", tt, "_xmpc")), na.rm = TRUE
    ),
    federal_transfers_net_taxes = rowSums(
      select(., paste0("federal_", tt, "_xmpc")), na.rm = TRUE
    )
  )


# Taxes, Transfers, & Subsidies Contributions ---------------------------------------------------------------------

## Calulate the taxes, transfers, and subsidies FIM

contributionTTS <- paste0(
  c('health', 'social_benefits', 
    'taxes_transfers', 'federal_taxes_transfers', 'state_taxes_transfers',
    'corporate', 'noncorp',
    'subsidies', 'federal_subsidies', 'state_subsidies'),
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
  rename(!!set_names(paste0(net, "_cont"), contributionTTS)) %>%
  ## Add contribution of subsidies to contribution of taxes and transfers
  mutate(
    taxes_transfers_cont = taxes_transfers_cont + subsidies_cont,
    federal_taxes_transfers_cont = federal_taxes_transfers_cont + federal_subsidies_cont,
    state_taxes_transfers_cont = state_taxes_transfers_cont + state_subsidies_cont
  )

# We forecast two years ahead
####change to 6 quarters ahead for COVID19 as of August 3rd 2020
#last_proj_date = fim$date[which(fim$date == last_hist_date) + 8]


# 4.6 Export data -------------------------------------------------------------------------------------------
firstDate <- "1999-12-31"
saveRDS(fim %>% filter(date > firstDate ), 'data/processed/fim.rds')
# 4.6.1 Clean FIM data frame ----------------------------------------------------------------------------
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
    select(year, quarter, recession, projection,
           fim_bars, fim_bars_ma, 
           federal_cont, state_local_cont,
           taxes_transfers_subsidies_cont) %>%
    rename(
      "total" = fim_bars,
      "impact" = fim_bars_ma,
      "federal" = federal_cont,
      "state_local" = state_local_cont,
      "consumption" = taxes_transfers_subsidies_cont
    )

  

# 4.6.3 Create CSV files --------------------------------------------------------------------------------

# Create folder for current month's update
thismonth <- format(Sys.Date(), "%m-%Y")
dir.create('results/', thismonth)

# Write csv to current month's folder
results <- 
  list(fim = fim,
                fim_interactive = fim_interactive,
                xx = xx)

list(data = results, 
     names = names(results)) %>%
  purrr::pmap(output_csv) 

