# pull data
#source("code/fim_datapull.R")

last_proj_date = as.Date(paste0(year(Sys.Date()) + 2, "-12-31"))
last_historical_date = tail(historical_quarterly$date, 1)

# calculate CBO's implicit price deflators for spendingFederal, spendingState, c
# growth rate of nominal over real

econ[,paste0("j",c("spendingFederal", "spendingState", "personalConsumptionNom"), "_g")] = 
  lapply(econ[,paste0("j",c("spendingFederal", "spendingState", "personalConsumptionNom"))], function(x) q_g(x))

# calculate quarterly rates
econ[,c(paste0(comp, "_g"))] = lapply(econ[,comp], function(x) q_g(x))

# construct forecasts of federal taxes and transfers growth using CBO's annual 
# budget/revenue projections as they appear in the NIPAs
#(except Medicaid and Medicare, which come straight from revenue projections)

# we're going to use annual rates anyhow, 
# so just replicate the annual levels over each q
budg = rbind(budg, budg, budg, budg) 
budg = budg[order(budg$fy),]
# shift budget projection date (FY) to match calendar date 
budg$date =
  shift(econ$date[which(as.integer(format(as.Date(econ$date, format="%d-%m-%Y"),"%Y")) %in% budg$fy)], 1, type=c("lag")) 


# adjust federal transfers to feature their january COLA-related bump; 
# reattribute that growth to calendar quarter 1 before smoothing out the 
# rest of the non-COLA related growth. SSA uses CPI-W to create COLAs; 
# we just take CBO's projection of CPI-U. That won't affect the level of 
# total transfers, just the timing a little bit
budg = budg %>% mutate(cpiu = econ$cpiu[match(budg$date, econ$date)], 
                       cpiu_g = q_a(cpiu)/100, 
                       pcw = historical_quarterly$pcw[match(budg$date, historical_quarterly$date)],
                       pcw_g = q_a(pcw)/100)

budg$pcw_g[is.na(budg$pcw_g)] = budg$cpiu_g[is.na(budg$pcw_g)]
budg = budg %>% mutate(cola_rate = ifelse(month(date) == 3, lag(cpiu_g,2), NA)) # applicable cola rate is CPIW from Q3 of previous year
for(i in 4:nrow(budg)){
  if(is.na(budg$cola_rate[i])){
    budg$cola_rate[i] = max(budg$cola_rate[i-1],0) # just forward filling so each of the quarters has its correct cola rate, remove negative values
  }
}

budg <- budg %>% mutate(health_ui = yptmd + yptmr + yptu,
                       health_ui = SMA(health_ui,4),
                       spendingFederaltfpnoCOLA = spendingFederaltfp - health_ui, # temporarily take out medicaid, medicare, and ui
                       spendingFederaltfpnoCOLA = spendingFederaltfpnoCOLA*(1-cola_rate), # take of the COLA portion
                       spendingFederaltfpnoCOLA = SMA(spendingFederaltfpnoCOLA, 4), # smooth that out
                       spendingFederaltfp_adj = spendingFederaltfpnoCOLA*(1+cola_rate), # add the cola back in
                       spendingFederaltfp_adj = spendingFederaltfp_adj + health_ui,# add smoothed health back in.
                       spendingFederaltfp_unadj = spendingFederaltfp, # storing the old one as "unadjusted"
                       spendingFederaltfp = spendingFederaltfp_adj, # store the new one as the series to use
                       spendingFederaltfp_g = q_g(spendingFederaltfp)) %>%
        select(-spendingFederaltfp_adj)

                
# smooth all budget series except total social transfers, which we did above
cc = c("spendingFederalrpt",  "spendingFederalrpri",  "spendingFederalrcp",  "spendingFederalrs", "yptmr",  "yptmd" ) #"spendingFederaltfp",  
budg[,cc] = sapply(budg[,cc], function(x) SMA(x, n=4))

# take "q-o-q" growth rate
budg[,c(paste0(cc, "_g"))] = lapply(budg[,cc], function(x) q_g(x))

# construct alternative scenario for personal current taxes, under which the TCJA provisions for income taxes don't expire in 2025
budg$spendingFederalrptb = budg$spendingFederalrpt # current law path
budg$spendingFederalrptb_g = budg$spendingFederalrpt_g # current law growth
expdate = "2025-12-30"
predate = "2025-09-30"
budg$spendingFederalrpt_g[which(budg$date >= expdate)] = budg$spendingFederalrpt_g[which(budg$date == predate)]

postdates = which(budg$date >= predate)
for(i in 2:length(postdates)){
  budg$spendingFederalrpt[postdates[i]] = budg$spendingFederalrpt[postdates[i-1]]*(1 + budg$spendingFederalrpt_g[postdates[i]]/400)
}

# construct forecasts of state and local taxes growth
aa <- plyr::rbind.fill(aa,econ_a)
forward_aa <- which(aa$date > last_historical_quarterly_date)
taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
taxpieces_gdp = paste0(taxpieces, "_gdp")
aa[,taxpieces_gdp] = lapply(aa[,taxpieces], function(x) x/aa$gdp)
aa[,taxpieces_gdp] = lapply(aa[,taxpieces_gdp], function(x) na.locf(x))
# aa[,taxpieces] = sapply(aa[,taxpieces_gdp], function(x) x*aa$gdp)

# translate into quarterly SAAR levels by replicating over four quarters and smoothing
aa = rbind(aa, aa, aa, aa) # we're going to use annual rates anyhow, so just replicate the annual levels over each q
aa = aa[order(aa$date),]
aa$date[format(aa$date, f="%Y") %in% format(econ$date, f="%Y")] = econ$date[format(econ$date, f="%Y") %in% format(aa$date, f="%Y")]

econ <- merge(aa[,c("date", taxpieces, taxpieces_gdp)], econ, by = "date", all.x = F)
econ[,taxpieces] = sapply(econ[,taxpieces_gdp], function(x) x*econ$gdp)


# calculate growth rates
econ[,paste0(taxpieces, "_g")] = lapply(econ[,taxpieces], function(x) q_g(x))

# Merge all projection dfs and generate projections of levels. We use projected growth rates and iteratively forecast levels from current levels. 
xx = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "date", all = TRUE),list(budg[,c("date", grep("_g", colnames(budg), value = T))], 
                                                                                 # aa[,c("date",grep("_g", colnames(aa), value = T))],
                                                                                 econ[,c("date", grep("_g", colnames(econ), value = T))], 
                                                                                 historical_quarterly))

forward = which(!(xx$date %in% historical_quarterly$date))

#reattribute state unemployment from federal back to state
#includes federal UI which we need to take out 768.8 [legislation total] from Q2 of state unemployment insurance
xx$spendingFederaltfbusx = xx$spendingFederaltfbusx/1000 #translate from millions to billions
xx$spendingFederaltfp = xx$spendingFederaltfp - xx$spendingFederaltfbusx # + 768.8
xx$spendingFederaltfp[202] = xx$spendingFederaltfp[202] + 768.8

xx$gstfp = xx$gstfp + xx$spendingFederaltfbusx # - 768.8
xx$gstfp[202] = xx$gstfp[202] - 768.8

# assume FMAP remains constant -- we still need the fmaps to do pre-1993 reallocation of grants
xx$fshare = fmap$fshare[match(as.integer(format(as.Date(xx$date, format="%d-%m-%Y"),"%Y")), fmap$year)]
xx$fshare = na.locf(xx$fshare)

## Louise override CBO growth rate for S&L purchases for Q3 and Q4 for 2020
xx$gs_g[203] = -0.05  
xx$gs_g[204] = 0.04
  
# Additional component calculations
xx <- xx %>% mutate(
  
  # Make special assumptions for projected growth rates
  spendingFederal_g = ifelse((date > "2021-09-30"), gdppothq_g + jgdp_g, spendingFederal_g), # past cap expiration dates, CBO assumes that fed purchases just grow with inflation. we want to assume they grow with nominal potential (zero impact, essentially)
  
  gstfp_g = gs_g, # state and local transfers grow with state and local current expenditures
  gstfpnet_g =  gs_g, # state and local transfers grow with state and local current expenditures
  spendingFederaltfpnet_g = spendingFederaltfp_g, # net federal transfers same as gross federal transfers

  spendingFederaleghhx_g = yptmd_g, # federal health grants to states grow with medicaid
  spendingFederaleghdx_g = yptmd_g, # federal medicaid grants to states grow with medicaid
  
  spendingFederaleg_g = spendingFederal_g, # federal current grants to state and local gov'ts grow with federal purchases
  spendingFederaleigx_g = spendingFederal_g, # federal capital grants to state and local gov'ts grow with federal purchases
  
  jgsi_g = jgs_g,
  jgse_g = jgs_g, # deflators for state & local investment, consumption grow with overall deflator
  
  spendingFederalsub_g = gdppothq_g,
  gssub_g = gdppothq_g # subsidies grow with potential
  
  # yfptmd_g = yptmd_g,  # disaggregated medicaid components grow with the aggregate
  # ysptmd_g = yptmd_g, # disaggregated medicaid components grow with the aggregate
)

# TEMPORARY ADJUSTMENT TO GROWTH RATES FOR FEDERAL GRANTS
# xx$spendingFederaleg_g[xx$date == "2020-03-31"] = xx$spendingFederaleg_g[xx$date == "2019-12-31"][1]
# xx$spendingFederaleigx_g[xx$date == "2020-03-31"] = xx$spendingFederaleg_g[xx$date == "2019-12-31"][1]

# generate forward values of components using current levels and projected growth rates. 
comp2 = c("gdph", "gdppothq", "gdp","personalConsumptionNom", "yptmr","yptmd","g","spendingFederal", "spendingFederaleg","spendingFederaleigx","spendingFederaleghhx", "spendingFederaleghdx", "spendingState", "spendingFederalrpt","spendingFederalrpri","spendingFederalrcp","spendingFederalrs","gsrpt","gsrpri","gsrcp","gsrs","spendingFederaltfp","gstfp","gdppotq","jgdp","jc","jspendingFederal","jgs","jgse", "jgsi", "gssub", "spendingFederalsub")
for(i in 1:length(comp2)){
  for(j in 1:length(forward)){
    xx[forward[j], comp2[i]] = xx[forward[j]-1, comp2[i]]*(1+xx[forward[j], paste0(comp2[i], "_g")])
  }
}

# Additional component calculations
xx <- xx %>%   mutate(
  
  # save some vars
  spendingFederalsave = spendingFederal, 
  gssave = spendingState, 
  spendingFederalrptsave = spendingFederalrpt, # preserve current law personal taxes, levels
  
  # fix units on some variables (millions -> billions)
  spendingFederaleghhx = spendingFederaleghhx / 1000,
  spendingFederaleghdx = spendingFederaleghdx / 1000,
  spendingFederaleigx = spendingFederaleigx / 1000,
  
  # Reattribute federal grants to states back to Federal government
  # Parse between those for consumption and investment and those for transfers (Medicaid)
  
  # federal medicaid grants to states
  yfptmd = ifelse(is.na(spendingFederaleghdx), # if we don't have the medicaid data (pre-1993)'
                  yptmd*fshare, # use the fmaps to estimate
                  spendingFederaleghdx), # otherwise, use data for medicaid + prescription drugs transfers
  
  # state medicaid payments = total medicaid - federal medicaid grants
  ysptmd = yptmd - yfptmd,
  spendingFederalegnet = spendingFederaleg - yfptmd, # federal grants to state and local net of medicaid grants
  
  # Reattribute federal Medicaid grants to states back to federal government, away from state and local transfer payments
  gstfpnet = gstfp - yfptmd, # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
  spendingFederaltfpnet = spendingFederaltfp + yfptmd # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
  
  # we reattribute the capital grants later after calculating contributions. 
  
) 

# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
xx$gtfp[forward] = xx$spendingFederaltfp[forward] + xx$gstfp[forward] # social benefits = federal benefits + state and local benefits
xx$yptx[forward] = xx$spendingFederalrpt[forward] + xx$gsrpt[forward] # alternative path
xx$yptxb[forward] = xx$spendingFederalrptb[forward] + xx$gsrpt[forward] # current law
xx$ytpi[forward] = xx$gsrpri[forward] + xx$spendingFederalrpri[forward]  #production and import taxes
xx$grcsi[forward] = xx$gsrs[forward] + xx$spendingFederalrs[forward]  # payroll taxes
xx$yctlg[forward] = xx$gsrcp[forward] + xx$spendingFederalrcp[forward] # corporate taxes
xx$gsub[forward] = xx$gssub[forward] + xx$spendingFederalsub[forward] # subsidies

