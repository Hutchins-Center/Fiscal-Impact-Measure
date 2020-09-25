source("fim_projections.R")
library(tidyr)
library(zoo)

writedata = T

# construct nice df for calculations

fim = data.frame(
  date = xx$date,
  
  # tax and transfers by level of government (all sources)
  state_health_outlays = xx$ysptmd, # only includes medicaid 
  state_social_benefits = xx$gstfpnet - xx$ysptmd, # no medicare at state and local level
  state_noncorp_taxes = xx$gsrpt + xx$gsrpri + xx$gsrs, 
  state_corporate_taxes = xx$gsrcp,
  
  federal_health_outlays = xx$yfptmd + xx$yptmr,
  federal_social_benefits = xx$gftfpnet - (xx$yfptmd + xx$yptmr),
  federal_noncorp_taxes = xx$gfrpt + xx$gfrpri + xx$gfrs, 
  federal_corporate_taxes = xx$gfrcp,
  
  # taxes and transfers category totals
  health_outlays = xx$yptmr + xx$yptmd , # Medicare + Medicaid
  social_benefits = xx$gtfp - (xx$yptmr + xx$yptmd), # Social benefits net health outlays
  noncorp_taxes = xx$yptx+xx$ytpi+xx$grcsi , # alternative
  corporate_taxes = xx$yctlg,  
  
  # consumption and investment totals
  federal_nom = xx$gf , # BEA line item
  federal_cgrants = xx$gfegnet, # Federal (non-health) grants in aid to states, nominal
  federal_igrants = xx$gfeigx, # Federal capital grants in aid to states, nominal
  pi_federal = q_g(xx$jgf),
  
  state_local_nom = xx$gs ,
  pi_state_local = q_g(xx$jgs) ,
  
  
  # other needed reference variables 
  gdp = xx$gdp , # nominal gdp
  gdppot = q_g(xx$gdppothq) + q_g(xx$jgdp) , #  nominal potential output growth
  # gdppoth = q_g(xx$gdppotq) - q_g(xx$jgdp), # real potential output growth
  gdppoth = q_g(xx$gdppothq), # real potential output growth
  pi_gdp = q_g(xx$jgdp), # gdp "deflator"
  pce = xx$c, # nominal consumption
  pi_pce = q_g(xx$jc),
  recession = xx$recessq + 1)

fim$recession[fim$recession == 2] = 1

# Federal purchases FIM
fim$federal_cont = NA
for(i in 2:nrow(fim)){
  fim$federal_cont[i] = 400*(fim$federal_nom[i] - (1 + fim$pi_federal[i] + fim$gdppoth[i])*fim$federal_nom[i-1])/fim$gdp[i-1]
}

# State and Local purchases FIM
fim$state_local_cont = NA
for(i in 2:nrow(fim)){
  fim$state_local_cont[i] = 400*(fim$state_local_nom[i] - (1 + fim$pi_state_local[i] + fim$gdppoth[i])*fim$state_local_nom[i-1])/fim$gdp[i-1]
}

# total purchases FIM

fim$purchases_cont = fim$state_local_cont + fim$federal_cont

# Taxes and Transfers FIM

# subtract counterfactual taxes and transfers from realized taxes
tt = c("health_outlays", "social_benefits", "noncorp_taxes", "corporate_taxes") # category totals
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

# Set MPC's 
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
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) # distributes out to 40 percent of the -0.6 MPC applied in first two quarters and the remainder evenly over last 5
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

# Take tax and transfer category totals, net of counterfactual taxes, multiply by MPC's
health = grep("health", tts, value=T)
social_benefits = grep("social_benefits", tts, value=T)
noncorp = grep("noncorp", tts, value=T)
corporate = grep("corporate", tts, value=T)

fim[,paste0(health, "_xmpc")] = lapply(fim[,health], function(x) mpc_health_outlays(x))
fim[,paste0(social_benefits, "_xmpc")] = lapply(fim[,social_benefits], function(x) mpc_social_benefits(x))
fim[,paste0(noncorp, "_xmpc")] = lapply(fim[,noncorp], function(x) mpc_noncorp_taxes(x))
fim[,paste0(corporate, "_xmpc")] = lapply(fim[,corporate], function(x) mpc_corporate_taxes(x))

# Sum up transfers net taxes
fim$transfers_net_taxes = rowSums(fim[,paste0(tt,"_xmpc")], na.rm = T)
fim$state_transfers_net_taxes = rowSums(fim[,paste0("state_",tt,"_xmpc")], na.rm = T)
fim$federal_transfers_net_taxes = rowSums(fim[,paste0("federal_",tt,"_xmpc")], na.rm = T)

# calulate the taxes and transfers FIM
tt_cont = c("taxes_transfers_cont", "state_taxes_transfers_cont", "federal_taxes_transfers_cont")
other_cont = c("health_cont", "social_benefits_cont", "noncorp_cont", "corporate_cont")

fim[,c(tt_cont, other_cont)] = lapply(fim[,c("transfers_net_taxes", "state_transfers_net_taxes", "federal_transfers_net_taxes", "health_outlays_net_xmpc", "social_benefits_net_xmpc", "noncorp_taxes_net_xmpc", "corporate_taxes_net_xmpc")], function(x){
  for(i in 2:length(x)){
    j[i] = 400*x[i]/fim$gdp[i-1]
  }
  j
})

# We forecast two years ahead
last_proj_date = fim$date[which(fim$date == last_hist_date) + 8]

# cleaner fim df
fim <- fim %>% 
  mutate(fim_bars = state_local_cont + federal_cont + taxes_transfers_cont, 
         fim_bars_ma = SMA(na.locf(fim_bars, na.rm = F), n = 4)) %>% 
  filter(date <= as.Date(last_proj_date)) %>%
  select(date, fim_bars, fim_bars_ma, state_local_cont, federal_cont, taxes_transfers_cont, recession, everything())


# this csv is tailored for the website interactive
fim_interactive <- fim %>% 
  filter(date >= "1999-12-31") %>% 
  mutate(projection = as.numeric(date > last_hist_date),
         yrq = as.yearqtr(date),
         recession = ifelse(is.na(recession), 0, recession),
         projection = ifelse(is.na(projection), 0, projection)) %>%
  separate(yrq, c("year", "quarter")) %>%
  select(year, quarter, fim_bars_ma, recession, fim_bars, federal_cont, state_local_cont, taxes_transfers_cont, projection) %>%
  rename("impact" = fim_bars_ma,"total" =  fim_bars, "state_local" = state_local_cont, "federal" =  federal_cont, "consumption" = taxes_transfers_cont)



# write CSVs

thismonth_folder <- as.character(paste0('../',format(as.yearmon(Sys.Date()), f = "%m-%Y")))
if (file.exists(thismonth_folder)){
 subdir <- thismonth_folder
  } else {
  dir.create(file.path(getwd(), thismonth_folder))
  subdir <- thismonth_folder
  }

if(writedata == T){
  write.csv(fim, paste0(subdir, '/fim-projections-', Sys.Date(), ".csv"))
  write.csv(fim_interactive, paste0(subdir, '/fim-interactive.csv'), row.names = F)
  write.csv(xx, paste0(subdir, '/xx-', Sys.Date(), ".csv"))
  save(fim, file = 'V:/mng/updates/dashboard_V2/data/fim.rds')
}

