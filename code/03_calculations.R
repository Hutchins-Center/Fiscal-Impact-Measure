#source("code/fim_projections.R")


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
         recession = recessq + 1,
         # TAXES AND TRANSFERS
         ## Federal
         state_health_outlays = ysptmd,
         state_social_benefits = gstfpnet - ysptmd, # no medicare at state and local level
         state_noncorp_taxes = gsrpt + gsrpri + gsrs, 
         state_corporate_taxes = gsrcp,
         state_subsidies = gssub,
         ## State
         federal_health_outlays = yfptmd + yptmr,
         federal_social_benefits = gftfpnet - (yfptmd + yptmr),
         federal_noncorp_taxes = gfrpt + gfrpri + gfrs, 
         federal_corporate_taxes = gfrcp,
         federal_subsidies = gfsub,
         ## Total
         health_outlays = yptmr + yptmd , # Medicare + Medicaid
         social_benefits = gtfp - (yptmr + yptmd), # Social benefits net health outlays
         noncorp_taxes = yptx+ytpi+grcsi , # alternative
         corporate_taxes = yctlg,
         subsidies  = gsub,
         # CONSUMPTION AND INVESTMENT
         ## Federal
         federal_nom = gf, # Purchases
         federal_cgrants = gfegnet, # Federal (non-health) grants in aid to states
         federal_igrants = gfeigx, # Federal capital grants in aid to states, nominal
         pi_federal = q_g(jgf),
         ## State
         state_local_nom = gs ,
         pi_state_local = q_g(jgs) ,
         pi_state_local_c = q_g(jgse) ,
         pi_state_local_i = q_g(jgsi) 
         )

writedata = T

fim2 = data.frame(
  date = xx$date,
  
  # tax and transfers by level of government (all sources)
  state_health_outlays = xx$ysptmd, # only includes medicaid 
  state_social_benefits = xx$gstfpnet - xx$ysptmd, # no medicare at state and local level
  state_noncorp_taxes = xx$gsrpt + xx$gsrpri + xx$gsrs, 
  state_corporate_taxes = xx$gsrcp,
  state_subsidies = xx$gssub,
  
  federal_health_outlays = xx$yfptmd + xx$yptmr,
  federal_social_benefits = xx$gftfpnet - (xx$yfptmd + xx$yptmr),
  federal_noncorp_taxes = xx$gfrpt + xx$gfrpri + xx$gfrs, 
  federal_corporate_taxes = xx$gfrcp,
  federal_subsidies = xx$gfsub,
  
  # taxes and transfers category totals
  health_outlays = xx$yptmr + xx$yptmd , # Medicare + Medicaid
  social_benefits = xx$gtfp - (xx$yptmr + xx$yptmd), # Social benefits net health outlays
  noncorp_taxes = xx$yptx+xx$ytpi+xx$grcsi , # alternative
  corporate_taxes = xx$yctlg,
  subsidies  = xx$gsub,
  
  # consumption and investment totals
  federal_nom = xx$gf, # BEA line item
  federal_cgrants = xx$gfegnet, # Federal (non-health) grants in aid to states, nominal. I.e, federal grants to state and local net of medicaid grants
  federal_igrants = xx$gfeigx, # Federal capital grants in aid to states, nominal
  pi_federal = q_g(xx$jgf),
  
  state_local_nom = xx$gs ,
  pi_state_local = q_g(xx$jgs) ,
  pi_state_local_c = q_g(xx$jgse) ,
  pi_state_local_i = q_g(xx$jgsi) ,
  
  # other needed reference variables 
  gdp = xx$gdp , # nominal gdp
  gdppot = q_g(xx$gdppothq) + q_g(xx$jgdp) , #  nominal potential output growth
  gdppoth = q_g(xx$gdppothq), # real potential output growth
  pi_gdp = q_g(xx$jgdp), # gdp "deflator"
  pce = xx$c, # nominal consumption
  pi_pce = q_g(xx$jc),
  recession = xx$recessq + 1)
#######


# 5.2 Add-ons  ------------------------------------------------------------------------------------------

# Created to adjust our forecasts for the coronavirus bills
# Remove when NIPAS are updated or new economic projections are released (whichever comes first)

fim_corona_hist1 <- fim %>%
  filter(date <= "2020-06-30")

fim_corona_hist1[202, 'federal_cgrants'] <- fim_corona_hist1[202, 'federal_cgrants'] + (-298)
# fim_corona_hist1[202, 'federal_corporate_taxes'] <- fim_corona_hist1[202, 'federal_corporate_taxes'] + (-49.05)

fim_corona_hist2 <- fim %>%
  filter(date > "2021-12-31")

fim_corona <- fim %>%
  filter(date > "2020-06-30" & date <= "2021-12-31") %>%
  mutate(
#  #introduce state and local add-ons
#      #New add-ons for Q3 2020 and beyond Calculations are in covid-19 changes/september/addons_comparison_newadds_sept.xlsx
      add_state_health_outlays = c(39.46,33.75,28.04,22.33,16.62,18.34),
      add_state_social_benefits =  c(319.53,32.39,38.99,44.43,21.6,-16.7),
      add_state_noncorp_taxes =  c(-54.82, -87.56, -114.78, -132.92, -155.13, -161.58),
      add_state_corporate_taxes = c(31.78, 30.14, 33.5, 34.45, 35.55, 36.31),
      # add_state_expenditures = c(0,	3.1,	3.1,	3.1,	3.1,	5),

  #introduce federal add-ons
      #New add-ons for Q3 2020 and beyond Calculations are in covid-19 changes/september/addons_comparison_newadds_sept.xlsx
  add_federal_health_outlays = c(-22.65,-5.57,-5.84,7.19,9.9,53.43),
  add_federal_social_benefits = c(-1490.39,-1546.12,-1429.51,-112.18,-899.73,-994.39),
  # add_federal_noncorp_taxes = c(31.51, 41.57, 55.78, 69.01, 75.67, 96.18),
  # add_federal_corporate_taxes = c(-4.09, -10.58, -5.48, -6.10, -6.16, -5.76),  #does not include -49.05 for Q2
  add_federal_subsidies = c(119.97,-715.67,-989.94,-1006.02,-1010.3,-1014.92),
  add_federal_grants = c(-534.23,-528.32,-530.05,-533.51,-519.94,-591.54), #missing -450 for Q2
  # add_federal_nom = c(-60, -60, -60, -60, -60, -60),


    #calculate new variables by adding the add factors
    state_health_outlays  = state_health_outlays + add_state_health_outlays,
    state_social_benefits  = state_social_benefits + add_state_social_benefits,
    state_noncorp_taxes  =  state_noncorp_taxes + add_state_noncorp_taxes,
    state_corporate_taxes  = state_corporate_taxes + add_state_corporate_taxes,
  #
    federal_health_outlays  = federal_health_outlays + add_federal_health_outlays,
    federal_social_benefits  = federal_social_benefits + add_federal_social_benefits,
  #   federal_noncorp_taxes  = federal_noncorp_taxes + add_federal_noncorp_taxes,
  #   federal_corporate_taxes  = federal_corporate_taxes + add_federal_corporate_taxes,
    federal_subsidies  = federal_subsidies + add_federal_subsidies,
  #
  #   #new category totals
    health_outlays  = state_health_outlays  + federal_health_outlays ,
    social_benefits  = state_social_benefits  + federal_social_benefits ,
    noncorp_taxes  = state_noncorp_taxes  + federal_noncorp_taxes ,
    corporate_taxes  = state_corporate_taxes  + federal_corporate_taxes ,
    subsidies   = state_subsidies + federal_subsidies,
    # state_local_nom = add_state_expenditures + state_local_nom,
    # federal_nom = add_federal_nom + federal_nom,
    federal_cgrants = add_federal_grants + federal_cgrants

  )

 fim_new <- bind_rows(fim_corona_hist1, fim_corona, fim_corona_hist2)
 fim <- fim_new
 rm(fim_corona, fim_new, fim_corona_hist1, fim_corona_hist2)
 
 fim$federal_cgrants[202] = 181.51 #Manually entering value for Q2 2020 since add factors start in Q3
 
#====

fim$recession[fim$recession == 2] = 1

# 4.3 Contributions -------------------------------------------------------------------------------------

contribution <- function(df, var){
  df %>%
    mutate(
      "{{ var }}_cont" := 400 * ({{ var }} - (1 + jgf + gdppothq) * lag({{ var }}) ) / lag(gdp)
      )
}
xx %>% contribution(gf) %>% select(gf, gf_cont)
# PURCHASES
## Federal 
fim$federal_cont = NA
for(i in 2:nrow(fim)){
  fim$federal_cont[i] = 400*(fim$federal_nom[i] - (1 + fim$pi_federal[i] + fim$gdppoth[i])*fim$federal_nom[i-1])/fim$gdp[i-1]
}

# S&L
fim$state_local_cont = NA
for(i in 2:nrow(fim)){
  fim$state_local_cont[i] = 400*(fim$state_local_nom[i] - (1 + fim$pi_state_local[i] + fim$gdppoth[i])*fim$state_local_nom[i-1])/fim$gdp[i-1]
}

# Sum up purchases FIM, taking out the federal grants contribution from state and local and adding it back to federal. 

fim = fim %>% mutate(
  
  federal_cont_0 = federal_cont,
  federal_cont = federal_cont + federal_cgrants_cont + federal_igrants_cont,
  
  state_local_cont_0 = state_local_cont,
  state_local_cont = state_local_cont - (federal_cgrants_cont + federal_igrants_cont),
  
  purchases_cont = state_local_cont + federal_cont
)

# GRANTS
## Federal "C" grants to states 
## Use S&L consumption deflator, since those entities are spending the money
fim$federal_cgrants_cont = NA
for(i in 2:nrow(fim)){
  fim$federal_cgrants_cont[i] = 400*(fim$federal_cgrants[i] - (1 + fim$pi_state_local_c[i] + fim$gdppoth[i])*fim$federal_cgrants[i-1])/fim$gdp[i-1]
}

## Federal "I" grants to states 
## Use S&L consumption deflator, since those entities are spending the money
fim$federal_igrants_cont = NA
for(i in 2:nrow(fim)){
  fim$federal_igrants_cont[i] = 400*(fim$federal_igrants[i] - (1 + fim$pi_state_local_i[i] + fim$gdppoth[i])*fim$federal_igrants[i-1])/fim$gdp[i-1]
}



#######


# 4.4 Taxes and Transfers -------------------------------------------------------------------------------


# subtract counterfactual taxes and transfers from realized taxes
tt = c("subsidies","health_outlays", "social_benefits", "noncorp_taxes", "corporate_taxes") # category totals
tts = c(tt, paste0("state_", tt), paste0("federal_", tt)) # totals and disaggregations by level of goverment
fim[,paste0(tts, "_net")] = y = lapply(fim[,tts], function(x){
  j = c()
  j[1] = x[1]
  for(i in 2:length(x)){
    j[i] = x[i] - x[i-1]*(1 + fim$pi_pce[i] + fim$gdppoth[i])
  }
  na.locf(j)
})

tts = paste0(tts, "_net") # rename for efficiency
tt = paste0(tt, "_net") # rename for efficiency


# 4.5 MPCs ----------------------------------------------------------------------------------------------


### 4.5.1 Pre-COVID -----------------------------------------------------------------------------------------

mpc_health_outlays = function(x){
  0.9*c(SMA(x, n=4))
}


mpc_social_benefits = function(x){
  0.9*c(SMA(x, n=4))
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
  0.9*c(SMA(x, n=4))
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

#comment out the old MPC functions  
  # fim[,paste0(health, "_xmpc")] = lapply(fim[,health], function(x) mpc_health_outlays(x))
  # fim[,paste0(social_benefits, "_xmpc")] =  lapply(fim[,social_benefits], function(x) mpc_social_benefits(x))
  # fim[,paste0(noncorp, "_xmpc")] = lapply(fim[,noncorp], function(x) mpc_noncorp_taxes(x))
  # fim[,paste0(corporate, "_xmpc")] =  lapply(fim[,corporate], function(x) mpc_corporate_taxes(x))
  # fim[,paste0(subsidies, "_xmpc")] = lapply(fim[,subsidies], function(x) mpc_subsidies(x))
  
#calculate the new mpc variables using the new functions only for the selected dates
  fim_MPC_crn19 <- fim %>%
    filter(date >= "2017-09-30" & date <= "2021-12-31") %>% #need to include 12 piror quarters to Q2 2020 for MPC calculation
    mutate_at(.funs = list("xmpc" = ~mpc_health_outlays_CRN19(.)), .vars = health) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_social_benefits_CRN19(.)), .vars = social_benefits) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_noncorp_taxes_CRN19(.)), .vars = noncorp) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_corporate_taxes_CRN19(.)), .vars = corporate) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_subsidies(.)), .vars = subsidies) %>%
    filter(date > "2020-03-31")

#for the historial dates use the old/pre-Corona MPC functions 
  fim_MPCold <- fim %>%
    filter(date <= "2020-03-31") %>%
    mutate_at(.funs = list("xmpc" = ~mpc_health_outlays(.)), .vars = health) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_social_benefits(.)), .vars = social_benefits) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_noncorp_taxes(.)), .vars = noncorp) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_corporate_taxes(.)), .vars = corporate) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_subsidies(.)), .vars = subsidies)

  fim_MPCold2 <- fim %>%
    filter(date >= "2017-09-30") %>%
    mutate_at(.funs = list("xmpc" = ~mpc_health_outlays(.)), .vars = health) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_social_benefits(.)), .vars = social_benefits) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_noncorp_taxes(.)), .vars = noncorp) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_corporate_taxes(.)), .vars = corporate) %>%
    mutate_at(.funs = list("xmpc" = ~mpc_subsidies(.)), .vars = subsidies) %>%
    filter(date > "2021-12-31")
  
#combine for new fim data frame   
    fim <- bind_rows(fim_MPCold, fim_MPC_crn19, fim_MPCold2)
######

# Sum up transfers net taxes
fim$transfers_net_taxes = rowSums(fim[,paste0(tt,"_xmpc")], na.rm = T)
fim$state_transfers_net_taxes = rowSums(fim[,paste0("state_",tt,"_xmpc")], na.rm = T)
fim$federal_transfers_net_taxes = rowSums(fim[,paste0("federal_",tt,"_xmpc")], na.rm = T)

# calulate the taxes, transfers, and subsidies FIM
tt_cont = c("taxes_transfers_cont", "state_taxes_transfers_cont", "federal_taxes_transfers_cont")
other_cont = c("health_cont", "social_benefits_cont", "noncorp_cont", "corporate_cont")
sub_cont = c("state_subsidies_cont", "federal_subsidies_cont", "subsidies_cont")

fim[,c(tt_cont, other_cont, sub_cont)] = lapply(fim[,c("transfers_net_taxes", "state_transfers_net_taxes", "federal_transfers_net_taxes", "health_outlays_net_xmpc", "social_benefits_net_xmpc", "noncorp_taxes_net_xmpc", "corporate_taxes_net_xmpc", "state_subsidies_net_xmpc", "federal_subsidies_net_xmpc", "subsidies_net_xmpc")], function(x){
  for(i in 2:length(x)){
    j[i] = 400*x[i]/fim$gdp[i-1]
  }
  j
})

# We forecast two years ahead
####change to 6 quarters ahead for COVID19 as of August 3rd 2020
last_proj_date = fim$date[which(fim$date == last_hist_date) + 6]


# 4.6 Export data -------------------------------------------------------------------------------------------

# 4.6.1 Clean FIM data frame ----------------------------------------------------------------------------


fim <- fim %>% 
  mutate(fim_bars = state_local_cont + federal_cont + taxes_transfers_cont + subsidies_cont, 
         fim_bars_ma = SMA(na.locf(fim_bars, na.rm = F), n = 4)) %>% 
  filter(date <= as.Date(last_proj_date)) %>%
  select(date, fim_bars, fim_bars_ma, state_local_cont, federal_cont, taxes_transfers_cont, 
         subsidies_cont, recession, everything())



# 4.6.2 Website interactive ----------------------------------------------------------------------------------
firstDate <- "1999-12-31"

fim_interactive <- 
  fim %>% 
    filter(date >= firstDate) %>% 
    mutate(projection = as.numeric(date > last_hist_date),
           yrq = as.yearqtr(date),
           recession = if_else(is.na(recession),
                               0,
                               recession),
           projection = if_else(is.na(projection),
                                0,
                                projection),
           taxes_transfers_subsidies_cont = taxes_transfers_cont + subsidies_cont) %>%
    separate(yrq, c("year", "quarter")) %>%
    select(year, quarter, fim_bars_ma, recession, fim_bars,
           federal_cont, state_local_cont, taxes_transfers_subsidies_cont, projection) %>%
    dplyr::rename("impact" = fim_bars_ma,"total" =  fim_bars, "state_local" = state_local_cont,
                  "federal" =  federal_cont, "consumption" = taxes_transfers_subsidies_cont)
  

# 4.6.3 Create CSV files --------------------------------------------------------------------------------

# Create folder for current month's update
thismonth_folder <- as.character(paste0(format(as.yearmon(Sys.Date()), f = "%m-%Y")))
if (file.exists(thismonth_folder)){
 subdir <- thismonth_folder
  } else {
  dir.create(file.path(getwd(), thismonth_folder))
  subdir <- thismonth_folder
  }

# Write csv to current month's folder
if(writedata == T){
  write.csv(fim, paste0(subdir, '/fim-projections-', Sys.Date(), ".csv"))
  write.csv(fim_interactive, paste0(subdir, '/fim-interactive-', Sys.Date(),".csv"), row.names = F)
  write.csv(xx, paste0(subdir, '/xx-', Sys.Date(), ".csv"))
  save(fim, file = 'V:/mng/updates/dashboard_V2/data/fim.rds')
}
