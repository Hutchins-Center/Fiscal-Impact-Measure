source('code/01_datapull.R')
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


# construct forecasts of federal taxes and transfers growth using CBO's annual budget/revenue projections 
# as they appear in the NIPAs (except Medicaid and Medicare, which come straight from revenue projections)

budg <- 
  # we use annual rates, so we can just replicate annual levesl for each q
  bind_rows(budg, budg, budg, budg) %>% 
  arrange(fy) %>%
  mutate(date = econ$date) %>%
  mutate(date = lag(date))

### 2.1.1 COLA Adjustments --------------------------------------------------------------------------------------

# Adjust federal transfers to feature their january COLA-related bump; 
# reattribute that growth to calendar quarter 1 before smoothing out the rest of the non-COLA related growth. 
# SSA uses CPI-W to create COLAs; We use CBO's projection of CPI-U. 
# This slightly affects the timing of total transfers, but not their levels

budg <- budg %>%
  mutate(cpiu = lag(econ$cpiu),
         cpiu_g = q_a(cpiu) / 100,
         pcw = hist$pcw[match(budg$date, hist$date)],
         pcw_g = q_a(pcw) / 100,
         #        Applicable cola rate is CPIW from Q3 of previous year
         cola_rate =
           case_when(
             month(date) == 3  ~ lag(cpiu_g, 2)
           )
  ) %>% 
  # forward filling so each q has correct cola rate, 
  fill(cola_rate)

# Don't think this does anything  
# budg$pcw_g[is.na(budg$pcw_g)] = budg$cpiu_g[is.na(budg$pcw_g)]

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

# Construct alternative scenario for personal current taxes, 
# under which the TCJA provisions for income taxes don't expire in 2025

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


# 2.2 Economic (Annual) ------------------------------------------------------------------------------------------
# 2.2.1 S&L Tax Growth -------------------------------------------------------------------------
# construct forecasts of state and local taxes growth

aa <- plyr::rbind.fill(aa, econ_a)
taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
aa <- aa %>%
  mutate(
    across(.cols  = taxpieces,
           .fns   = ~ na.locf(. / gdp)
    )
  ) 

# Translate into quarterly Seasonally Adjusted Annualized Rates levels 
# by replicating over four quarters and smoothing

firstDateEcon <- 
  econ %>%
  select(date) %>%
  slice_head()

aa <- 
  rbind(aa, aa, aa, aa) %>%
  arrange(date) %>%
  filter(date > firstDateEcon) %>% 
  mutate(date = econ$date) %>% 
  as_tibble()


## 3.1 Economic (Quarterly) ----------------------------------------------------------------------------------------------

### 3.1.1 ---------------------------------------------------------------------------
taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")

econ <-
  econ %>%
  # Implicit price deflators
  mutate(jgf =  gf/gfh,
         jgs = gs/gsh,
         jc = c/ch)  %>%
  # Growth rate of implicit price deflators
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ q_g(.x),
      .names = "{.col}_g"
    )
  ) %>%
  # S&L Taxes
  merge(aa %>%
          select(date, taxpieces),
        by = 'date',
        all.x = F) %>%
  mutate(
    across(
      .cols = taxpieces,
      .fns = ~ .x * gdp 
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