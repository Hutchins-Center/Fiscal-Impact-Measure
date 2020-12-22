# ## HEADER ---------------------------
# ##
# ## Script name: Projections
# ##
# ## Purpose of script:
# ## Construct quarterly projections
# ## Authors:
# ## Manuel Alcalá Kovalski
# ## Sage Belz
# ## Kadija Yilla
# ## Date Created: 2020-10-05
# ##
# ##
# ##
# ##
# ## Notes ---------------------
# ## We use projected growth rates and iteratively forecast levels from current levels.
# ##
# ##
#
# # 2 Projected Growth Rates -----------------------------------------------------------------------------
last_hist_date <-
  hist %>%
  select(date) %>%
  filter(!is.na(date)) %>%
  slice_tail() %>%
  pull()

last_proj_date <- last_hist_date + years(2)

taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
taxpieces_gdp = paste0(taxpieces, '_gdp')
## 2.1 Budget (Annual) ------------------------------------------------------------------------------------------


# construct forecasts of federal taxes and transfers growth using CBO's annual budget/revenue projections as they appear
# in the NIPAs (except Medicaid and Medicare, which come straight from revenue projections)

budg <-
  # we use annual rates, so we can just replicate annual levesl for each q
  bind_rows(budg, budg, budg, budg) %>%
  arrange(fy) %>%
  mutate(date = econ$date) %>%
  mutate(date = lag(date))



### 2.1.1 COLA Adjustments --------------------------------------------------------------------------------------

# Adjust federal transfers to feature their january COLA-related bump; reattribute that growth to calendar quarter 1
# before smoothing out the rest of the non-COLA related growth. SSA uses CPI-W to create COLAs; We use CBO's projection
# of CPI-U. This slightly affects the timing of total transfers, but not their levels

budg <- budg %>%
  mutate(cpiu = lag(econ$cpiu),
         cpiu_g = q_a(cpiu) / 100,
         pcw = hist$pcw[match(budg$date, hist$date)],
         pcw_g = q_a(pcw) / 100,
#        Applicable cola rate is CPIW from Q3 of previous year
         cola_rate =
           if_else(
             month(date) == 3,
             lag(cpiu_g, 2),
             NULL
           )
  ) %>%
  # forecastPeriod filling so each q has correct cola rate,
  fill(cola_rate)

# Don't think this does anything
budg$pcw_g[is.na(budg$pcw_g)] = budg$cpiu_g[is.na(budg$pcw_g)]

budg <-
  budg %>% mutate(health_ui = SMA(yptmd + yptmr + yptu, n = 4),
                        # temporarily take out medicaid, medicare, ui, and COLA
                        # smooth with 4 quarter  moving average
                        gftfp_noCOLA = SMA((gftfp - health_ui)*(1-cola_rate), n = 4),
                        # Store old gftfp as unadjusted
                        gftfp_unadj = gftfp,
                        # Add COLA and smoothed health back in for new adjusted gftfp
                        gftfp = gftfp_noCOLA * (1 + cola_rate)  + health_ui,
                        gftfp_g = q_g(gftfp)
                   ) %>%
            # smooth all budget series except total social transfers, which we did above
            mutate(
              across(.cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd" ),
                     .fns = ~ rollapply(.x, width = 4, mean, fill = NA, align =  'right')
                  ) %>%
            # take "q-o-q" growth rate
            mutate(
              across(
                .cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd" ),
                .fns  = ~  q_g(.x),
                .names = "{.col}_g"
              )
            )
          )



### 2.1.2 Alternate tax scenario --------------------------------------------------------------------------------

# Construct alternative scenario for personal current taxes, under which the TCJA provisions for income taxes don't
# expire in 2025

expdate <- "2025-12-30"
predate <- "2025-09-30"

budg <-
  budg %>%
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
                            gfrpt)
)

## 3.1 Economic (Quarterly) ----------------------------------------------------------------------------------------------

### 3.1.1 ---------------------------------------------------------------------------
taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
taxpieces_gdp = paste0(taxpieces, '_gdp')
# econ_a <-
#   read_xlsx('data/raw/cbo/cbo_econ_proj_annual.xlsx')  %>%
#   rename(date = calendar_date)
# 
# aa <- left_join(econ_a,
#       hist %>% filter(month(date) == 12) %>%
#         mutate(date = year(date)) %>%
#         select(date, taxpieces),
#       by = 'date')
# 
# aa <-
#   bind_rows(aa, aa, aa, aa) %>%
#   arrange(date) %>%
#   mutate(date = econ$date)
# 
# 
# aa <- aa %>%
#   mutate(across(
#     .cols = all_of(taxpieces),
#     .fns = ~ na.locf(. / gdp),
#     .names = "{col}_gdp"
#   ))
# 
# econ[,taxpieces] <- sapply(aa[,taxpieces_gdp], function(x) x*econ$gdp)

econ <-
  econ %>%
  # Implicit price deflators
  mutate(jgf =  gf/gfh,
         jgs = gs/gsh,
         jc = c/ch)  %>%
  # Growth rates
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ q_g(.x),
      .names = "{.col}_g"
    )
  ) %>%
  # S&L Taxes
  # Commented out what I think we should be doing instead for state taxes
 left_join(hist %>%
          select(date, taxpieces),
        all.x = F) %>%
  mutate(
    across(
      .cols = taxpieces,
      .fns = ~ na.locf(. / gdp) * gdp
    )
  ) %>%
 # Growth rate of S&L Taxes
 mutate(
   across(
     .cols = taxpieces,
     .fns = ~ q_g(.),
     .names = "{.col}_g"
   ) 
  ) %>%
    as_tibble()
## 4 Merge growth rates and levels data frames ---------------------------------------------------------------------

econGrowthRates <-
  econ %>%
  select(date, ends_with('_g'))

# Filter so that we only get the budget growth rates
budgetGrowthRates <-
  budg %>%
  select(date, ends_with('_g'), gfrptCurrentLaw)

growthRates <- left_join(econGrowthRates,
                         budgetGrowthRates,
                         by = 'date')

xx <-
    full_join(hist,
              growthRates,
              by = 'date')

## 4.2 FIM component calculations ----------------------------------------------------------------------------

### 4.2.1 ------------------------------------------------------------------------------------------
unemployment_insurance_override <-
  read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6.xlsx", 
           sheet = "FIM Add Factors") %>%
  mutate(date = as_date(date)) %>%
  select(date, contains('unemployment_insurance')) 
  

xx %<>%
  left_join(unemployment_insurance_override) %>%
  mutate(
    across(
      .cols = ends_with('override'),
      .fns = ~ if_else(is.na(.x), 0, .x)
    )
  )
  
# gftfbusx =	Fed Transfer Payments/Persons: State Unemployment Insurance Benefits (SAAR, Mil.$)
# gftfp = Federal Government Social Benefit Payments to Persons (SAAR, Bil.$)
# gstfp = State & Local Government Social Benefit Payments to Persons (SAAR, Bil.$)

# Federal UI legislation total from Q3 of 2020 is $768.8 (Bil. $)
xx <- 
  xx %>%
    mutate(
      
      # SOCIAL BENEFITS
      # Unemployment Insurance
      gftfbusx = gftfbusx / 1000, # Translate UI from millions to billions
      # Reallocate state UI (gftfbusx) from federal to state
      # Add ui legislation to federal and subtract from state
      gftfp = gftfp - gftfbusx + federal_unemployment_insurance_override,
      gstfp = gstfp + gftfbusx - federal_unemployment_insurance_override,
     
      # GRANTS
      
      # Fix units  (millions -> billions)
      # Health & Hospitals grants
      gfeghhx = gfeghhx / 1000,
      # Medicaid grants
      gfeghdx = gfeghdx / 1000,
      # Capital grants to S&L
      gfeigx = gfeigx / 1000,
      
        ## Medicaid
      
      # assume FMAP remains constant -- we still need the fmaps to do pre-1993 reallocation of grants
      fshare = fmap$fshare[match(year(date), fmap$year)] %>%
        na.locf()
    )


# 4.2.3 Growth Rates Assumptions -----------------------------------------------------------------------------------------


## Louise override CBO growth rate for S&L purchases for Q3 and Q4 for 2020

Q3_2020 <- "2020-09-30" 
Q4_2020 <- "2020-12-31" 
## Louise override CBO growth rate for S&L purchases for Q42020 through (& including) Q12022
xx$gs_g[204:209] = c(0.0025,0.0025,0.0025,0.005,0.0075,0.01)

# past cap expiration dates, CBO assumes that fed purchases just grow with inflation. we want to assume they grow with
# nominal potential (zero impact, essentially)
capExpiration <- "2021-09-30"

# Additional component calculations
# Make special assumptions for projected growth rates
xx <- xx %>% mutate(
  # PURCHASES
    # Federal
    gf_g = if_else(date > capExpiration,
                   gdppothq_g + jgdp_g,
                   gf_g),
    # State & Local
      # Note: Louise said to override CBO growth rate for S&L purchases for Q3 and Q4 for 2020

  # TRANSFERS
  
    # Federal
      # Net federal transfers grow at the same rate as gross federal transfers
    gftfpnet_g = gftfp_g, 
  
    # State & Local
      # S&L gross and net transfers both grow with S&L current expenditures
    gstfp_g = gs_g,
    gstfpnet_g =  gs_g, 

  # SUBSIDIES
    # Federal and S&L subsidies grow with potential GDP
  gfsub_g = gdppothq_g,
  gssub_g = gdppothq_g,
  
  # GRANTS
    # Federal
      #  Health & Hospital grants to states growth with Medicaid
        gfeghhx_g = yptmd_g, 
      # Medicaid grants to states grow with medicaid
        gfeghdx_g = yptmd_g, 
  
      # Aid to S&L grow with federal purchases
        gfeg_g = gf_g, 
      # Capital grants to state and local gov'ts grow with federal purchases
        gfeigx_g = gf_g, 
  
  # DEFLATORS
    # State & Local
        # Investment deflator grows with overall deflator for S&L
        jgsi_g = jgs_g,
        # Consumption deflator grows with overall deflator for S&L
        jgse_g = jgs_g
  
  # Disaggregated medicaid components grow with the aggregate  
      # yfptmd_g = yptmd_g,  
      # ysptmd_g = yptmd_g, 
)


# 5 Forecast ----------------------------------------------------------------------------------------------------

## Generate forecastPeriod values of components using current levels and our projected growth rates. 

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

forecastPeriod <- which(xx$date > last_hist_date)

for(f in forecastPeriod){
  xx[f,components] = xx[f-1, components]  * (1 + xx[f, paste0(components, "_g")])
}
# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
xx <-
  xx %>%
    mutate(
      
      # 
      # gtfp = gftfp + gstfp, # social benefits = federal benefits + state and local benefits
      # yptx = gfrpt + gsrpt, # alternative path
      # yptxb = gfrptCurrentLaw + gsrpt, # current law
      # ytpi = gsrpri + gfrpri,  #production and import taxes
      # grcsi = gsrs + gfrs,  # payroll taxes
      # yctlg = gsrcp + gfrcp, # corporate taxes
      # gsub = gssub + gfsub,
      
      
      # Reattribute federal grants to states back to Federal government
      # Parse between those for consumption and investment and those for transfers (Medicaid)
      
      # federal medicaid grants to states
      yfptmd = if_else(is.na(gfeghdx), # if we don't have the medicaid data (pre-1993)'
                       yptmd*fshare, # use the fmaps to estimate
                       gfeghdx), # otherwise, use data for medicaid + prescription drugs transfers
      
      
      # state medicaid payments = total medicaid - federal medicaid grants
      ysptmd = yptmd - yfptmd,
      gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
      
      # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
      gstfpnet = gstfp - yfptmd, 
      # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
      gftfpnet = gftfp + yfptmd 
      # we reattribute the capital grants later after calculating contributions. 
    )
xx$gtfp[forecastPeriod] = xx$gftfp[forecastPeriod] + xx$gstfp[forecastPeriod] # social benefits = federal benefits + state and local benefits
xx$yptx[forecastPeriod] = xx$gfrpt[forecastPeriod] + xx$gsrpt[forecastPeriod] # alternative path
xx$yptxb[forecastPeriod] = xx$gfrptCurrentLaw[forecastPeriod] + xx$gsrpt[forecastPeriod] # current law
xx$ytpi[forecastPeriod] = xx$gsrpri[forecastPeriod] + xx$gfrpri[forecastPeriod]  #production and import taxes
xx$grcsi[forecastPeriod] = xx$gsrs[forecastPeriod] + xx$gfrs[forecastPeriod]  # payroll taxes
xx$yctlg[forecastPeriod] = xx$gsrcp[forecastPeriod] + xx$gfrcp[forecastPeriod] # corporate taxes
xx$gsub[forecastPeriod] = xx$gssub[forecastPeriod] + xx$gfsub[forecastPeriod] # subsidies

