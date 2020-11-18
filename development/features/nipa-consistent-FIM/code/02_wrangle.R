## HEADER ---------------------------
##
## Script name: Projections
##
## Purpose of script:
## Construct quarterly projections
## Authors: 
## Manuel Alcalá Kovalski
## Sage Belz
## Kadija Yilla
## Date Created: 2020-10-05
##
##
##
##
## Notes ---------------------
## We use projected growth rates and iteratively forecast levels from current levels. 
##
## 

# 2 Projected Growth Rates -----------------------------------------------------------------------------
last_hist_date <-
  hist %>% 
  select(date) %>% 
  slice_tail() %>% 
  pull()
last_proj_date <- last_hist_date + years(2)
## 2.1 Budget (Annual) ------------------------------------------------------------------------------------------
# Construct forecasts of Federal taxes & transfers using CBO's annual budget projections.
# 
# Convert annual data to quarterly
budg <- 
  # we use annual rates, so we can just replicate annual levesl for each q
  bind_rows(budg, budg, budg, budg) %>% 
  arrange(fy) %>%
  mutate(date = econ$date) %>%
  mutate(date = lag(date))
### 2.1.1 COLA Adjustments --------------------------------------------------------------------------------------

# Adjust federal transfers to feature their january COLA-related bump; reattribute that growth to
# calendar quarter 1 before smoothing out the rest of the non-COLA related growth. SSA uses CPI-W to
# create COLAs; We use CBO's projection of CPI-U. This slightly affects the timing of total
# transfers, but not their levels

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


### 2.1.2 Growth rates --------------------------------------------------------------------------
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

expdate = "2025-12-30"
predate = "2025-09-30"

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
econ_a <-
  read_xlsx('data/raw/cbo/cbo_econ_proj_annual.xlsx')  %>%
  rename(date = calendar_date)

aa <- left_join(econ_a,
      hist %>% filter(month(date) == 12) %>% 
        mutate(date = year(date)) %>%
        select(date, taxpieces),
      by = 'date')

aa <-
  bind_rows(aa, aa, aa, aa) %>%
  arrange(date) %>%
  mutate(date = econ$date)


aa <- aa %>%
  mutate(across(
    .cols = all_of(taxpieces),
    .fns = ~ na.locf(. / gdp),
    .names = "{col}_gdp"
  ))

econ[,taxpieces] <- sapply(aa[,taxpieces_gdp], function(x) x*econ$gdp)

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
 # left_join(hist %>%
 #          select(date, taxpieces),
 #        all.x = F) %>%
 #  mutate(
 #    across(
 #      .cols = taxpieces_gdp,
 #      .fns = ~ na.locf(. / gdp) * gdp 
 #    )
 #  ) %>%
  # Growth rate of S&L Taxes
    # mutate(
    #   across(
    #     .cols = taxpieces,
    #     .fns = ~ q_g(.),
    #     .names = "{.col}_g"
    #   )
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


# Fix units (Millions to billions) -----------------------------------------------------------------------------------

# gftfbusx =	Fed Transfer Payments/Persons: State Unemployment Insurance Benefits (SAAR, Mil.$)
# gftfp = Federal Government Social Benefit Payments to Persons (SAAR, Bil.$)
# gstfp = State & Local Government Social Benefit Payments to Persons (SAAR, Bil.$)

# Federal UI legislation total from Q2 of 2020 is $768.8 (Bil. $)
Q2_2020 <- "2020-06-30" 
federal_UI <- 768.8
xx <- 
  xx %>%
  mutate(
    
    # SOCIAL BENEFITS
    # Unemployment Insurance
    # gftfbusx = gftfbusx / 1000,  Translate UI from millions to billions
    
    # Leave allocation of UI spending as is.
    # gftfp = gftfp - gftfbusx,
    # gstfp = gstfp + gftfbusx,
    # gftfp = if_else(date == Q2_2020,
    #                 gftfp + federal_UI,
    #                 gftfp),
    # gstfp = if_else(date == Q2_2020,
    #                 gstfp - federal_UI,
    #                 gstfp),
    
    # GRANTS
    
    # Fix units  (millions -> billions)
    # Health & Hospitals grants
    gfeghhx = gfeghhx / 1000,
    # Medicaid grants
    gfeghdx = gfeghdx / 1000,
    # Capital grants to S&L
    gfeigx = gfeigx / 1000,
    
    ## Medicaid
    # REMOVE RE-ALLOCATION OF MEDICARE. DO NOT NEED FMAP
    # assume FMAP remains constant -- we still need the fmaps to do pre-1993 reallocation of grants
    #   fshare = fmap$fshare[match(year(date), fmap$year)] %>%
    #     na.locf()
  )
