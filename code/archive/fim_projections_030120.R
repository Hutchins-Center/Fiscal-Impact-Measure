# pull data
source("fim_datapull.R")

last_proj_date = as.Date(paste0(year(Sys.Date()) + 2, "-12-31"))
last_hist_date = tail(hist$date, 1)
#last_proj_date = as.Date("2021-09-30")
#last_hist_date = as.Date("2019-09-30")


# calculate CBO's implicit price deflators for gf, gs, c
# growth rate of nominal over real
econ[,paste0("j",c("gf", "gs", "c"))] = lapply(c("gf", "gs", "c"), function(x){
  econ[,x]/econ[,paste0(x,"h")]
})
econ[,paste0("j",c("gf", "gs", "c"), "_g")] = lapply(econ[,paste0("j",c("gf", "gs", "c"))], function(x) q_g(x))

# TEMPORARY ADD-ONS FOR THE DEC 2019 H.R. 1865 APPROPRIATIONS BILL
# this is very hacky :(
# see 'Copy of Dec 2019 Legislation.xls' worksheet for calculations
approq <- filter(econ, date <= "2022-09-30" & date >= "2020-01-01") %>% select(date) %>%
  mutate(fy = c(rep(2020,3),rep(2021,4),rep(2022,4)))

econ$fy = approq$fy[match(econ$date, approq$date)]
econ$fy[is.na(econ$fy)] = 0

# econ$gf = econ$gf +  as.numeric(econ$fy == 2020)*13.645
#econ$gf[econ$fy == 2020] = econ$gf[econ$fy == 2020] + c(10,14,14)
#econ$gf = econ$gf +  as.numeric(econ$fy == 2021)*14.215
#econ$gf = econ$gf +  as.numeric(econ$fy == 2022)*14.801

# calculate quarterly rates
econ[,c(paste0(comp, "_g"))] = lapply(econ[,comp], function(x) q_g(x))

# construct forecasts of federal taxes and transfers growth using CBO's annual budget/revenue projections as they appear in the NIPAs (except Medicaid and Medicare, which come straight from revenue projections)

# TEMPORARY ADD-ONS FOR THE DEC 2019 H.R. 1865 APPROPRIATIONS BILL
# see 'Copy of Dec 2019 Legislation.xls' worksheet for calculations
# We put the revenue add-ons from Divisions N, O, and Q in manually here
addons <- budg - budg
addons <- addons %>%
mutate(#gfrpri = c(rep(0,3),-1589,	-15912,	-23417,rep(0,8))/1000, # excise taxes
      gfrpt = c(rep(0,4),	-11.62948286,	-15.5894,rep(0,8)), # other Division O and Q taxes
     gfrcp = c(rep(0,4),	-11.62948286,	-15.5894,rep(0,8))) # other Division Q taxes
#yptmd = c(rep(0,3),801+1645,762+1679 ,710+1714,rep(0,8))/1000, # medicaid + "other" (Using CBO numbers for medicare and medicaid)
#yptmr = c(rep(0,3),2431,2848,72,rep(0,8))/1000) # medicare
budg = budg + addons

budg = rbind(budg, budg, budg, budg) # we're going to use annual rates anyhow, so just replicate the annual levels over each q
budg = budg[order(budg$fy),]
# shift budget projection date (FY) to match calendar date 
budg$date = shift(econ$date[which(as.integer(format(as.Date(econ$date, format="%d-%m-%Y"),"%Y")) %in% budg$fy)], 1, type=c("lag")) 


# adjust federal transfers to feature their january COLA-related bump; reattribute that growth to calendar quarter 1 before smoothing out the rest of the non-COLA related growth. SSA uses CPI-W to create COLAs; we just take CBO's projection of CPI-U. That won't affect the level of total transfers, just the timing a little bit
budg = budg %>% mutate(cpiu = econ$cpiu[match(budg$date, econ$date)], 
                       cpiu_g = q_a(cpiu)/100, 
                       pcw = hist$pcw[match(budg$date, hist$date)],
                       pcw_g = q_a(pcw)/100)

budg$pcw_g[is.na(budg$pcw_g)] = budg$cpiu_g[is.na(budg$pcw_g)]
budg = budg %>% mutate(cola_rate = ifelse(month(date) == 3, lag(cpiu_g,2), NA)) # applicable cola rate is CPIW from Q3 of previous year
for(i in 4:nrow(budg)){
  if(is.na(budg$cola_rate[i])){
    budg$cola_rate[i] = max(budg$cola_rate[i-1],0) # just forward filling so each of the quarters has its correct cola rate, remove negative values
  }
}

budg$health_ui <- budg$yptmd + budg$yptmr + budg$yptu
budg$health_ui <- SMA(budg$health_ui,4)
budg$gftfpnoCOLA <- budg$gftfp - budg$health_ui
budg$gftfpnoCOLA <- budg$gftfpnoCOLA*(1-budg$cola_rate)
budg$gftfpnoCOLA <- SMA(budg$gftfpnoCOLA,4)
budg$gftfp_adj <- budg$gftfpnoCOLA*(1+budg$cola_rate)
budg$gftfp_adj <- budg$gftfp_adj + budg$health_ui
budg$gftf_unadj <- budg$gftfp
budg$gftfp <- budg$gftfp_adj
budg$gftfp_g <- q_g(budg$gftfp)


manu <- budg %>% mutate(health_ui = yptmd + yptmr + yptu,
                       health_ui = SMA(health_ui,4),
                       gftfpnoCOLA = gftfp - health_ui, # temporarily take out medicaid, medicare, and ui
                       gftfpnoCOLA = gftfpnoCOLA*(1-cola_rate), # take of the COLA portion
                       gftfpnoCOLA = SMA(gftfpnoCOLA, 4), # smooth that out
                       gftfp_adj = gftfpnoCOLA*(1+cola_rate), # add the cola back in
                       gftfp_adj = gftfp_adj + health_ui,# add smoothed health back in.
                       gftfp_unadj = gftfp, # storing the old one as "unadjusted"
                       gftfp = gftfp_adj, # store the new one as the series to use
                       gftfp_g = q_g(gftfp)) %>% 
        select(-gftfp_adj)  

                
# smooth all budget series except total social transfers, which we did above
cc = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd" ) #"gftfp",  
budg[,cc] = sapply(budg[,cc], function(x) SMA(x, n=4))

# take "q-o-q" growth rate
budg[,c(paste0(cc, "_g"))] = lapply(budg[,cc], function(x) q_g(x))

# construct alternative scenario for personal current taxes, under which the TCJA provisions for income taxes don't expire in 2025
budg$gfrptb = budg$gfrpt # current law path
budg$gfrptb_g = budg$gfrpt_g # current law growth
expdate = "2025-12-30"
predate = "2025-09-30"
budg$gfrpt_g[which(budg$date >= expdate)] = budg$gfrpt_g[which(budg$date == predate)]

postdates = which(budg$date >= predate)
for(i in 2:length(postdates)){
  budg$gfrpt[postdates[i]] = budg$gfrpt[postdates[i-1]]*(1 + budg$gfrpt_g[postdates[i]]/400)
}

# construct forecasts of state and local taxes growth
aa <- rbind.fill(aa,econ_a)
forward_aa <- which(aa$date > last_hist_date)
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
                                                                                 hist))
forward = which(!(xx$date %in% hist$date))

# assume FMAP remains constant -- we still need the fmaps to do pre-1993 reallocation of grants
xx$fshare = fmap$fshare[match(as.integer(format(as.Date(xx$date, format="%d-%m-%Y"),"%Y")), fmap$year)]
xx$fshare = na.locf(xx$fshare)

# Additional component calculations
xx <- xx %>% mutate(
  
  # Make special assumptions for projected growth rates
  gf_g = ifelse((date > "2021-09-30"), gdppothq_g + jgdp_g, gf_g), # past cap expiration dates, CBO assumes that fed purchases just grow with inflation. we want to assume they grow with nominal potential (zero impact, essentially)
  
  gstfp_g = gs_g, # state and local transfers grow with state and local current expenditures
  gstfpnet_g =  gs_g, # state and local transfers grow with state and local current expenditures
  gftfpnet_g = gftfp_g, # net federal transfers same as gross federal transfers

  gfeghhx_g = yptmd_g, # federal health grants to states grow with medicaid
  gfeghdx_g = yptmd_g, # federal medicaid grants to states grow with medicaid
  
  gfeg_g = gf_g, # federal current grants to state and local gov'ts grow with federal purchases
  gfeigx_g = gf_g, # federal capital grants to state and local gov'ts grow with federal purchases
  
  jgsi_g = jgs_g,
  jgse_g = jgs_g # deflators for state & local investment, consumption grow with overall deflator
  
  # yfptmd_g = yptmd_g,  # disaggregated medicaid components grow with the aggregate
  # ysptmd_g = yptmd_g, # disaggregated medicaid components grow with the aggregate
)

# TEMPORARY ADJUSTMENT TO GROWTH RATES FOR FEDERAL GRANTS
xx$gfeg_g[xx$date == "2020-03-31"] = xx$gfeg_g[xx$date == "2019-12-31"][1]
xx$gfeigx_g[xx$date == "2020-03-31"] = xx$gfeg_g[xx$date == "2019-12-31"][1]

# generate forward values of components using current levels and projected growth rates. 
comp = c("gdph", "gdppothq", "gdp","c", "yptmr","yptmd","g","gf", "gfeg","gfeigx","gfeghhx", "gfeghdx", "gs", "gfrpt","gfrpri","gfrcp","gfrs","gsrpt","gsrpri","gsrcp","gsrs","gftfp","gstfp","gdppotq","jgdp","jc","jgf","jgs","jgse", "jgsi")
for(i in 1:length(comp)){
  for(j in 1:length(forward)){
    xx[forward[j], comp[i]] = xx[forward[j]-1, comp[i]]*(1+xx[forward[j], paste0(comp[i], "_g")])
  }
}

# Additional component calculations
xx <- xx %>%   mutate(
  
  # save some vars
  gfsave = gf, 
  gssave = gs, 
  gfrptsave = gfrpt, # preserve current law personal taxes, levels
  
  # fix units on some variables (millions -> billions)
  gfeghhx = gfeghhx / 1000,
  gfeghdx = gfeghdx / 1000,
  gfeigx = gfeigx / 1000,
  
  # Reattribute federal grants to states back to Federal government
  # Parse between those for consumption and investment and those for transfers (Medicaid)
  
  # federal medicaid grants to states
  yfptmd = ifelse(is.na(gfeghdx), # if we don't have the medicaid data (pre-1993)'
                  yptmd*fshare, # use the fmaps to estimate
                  gfeghhx), # otherwise, use data for medicaid + prescription drugs transfers
  
  # state medicaid payments = total medicaid - federal health grants
  ysptmd = yptmd - yfptmd,
  gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
  
  # Reattribute federal Medicaid grants to states back to federal government, away from state and local transfer payments
  gstfpnet = gstfp - yfptmd, # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
  gftfpnet = gftfp + yfptmd # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
  
  # we reattribute the capital grants later after calculating contributions. 
  
) 

# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
xx$gtfp[forward] = xx$gftfp[forward] + xx$gstfp[forward] # social benefits = federal benefits + state and local benefits
xx$yptx[forward] = xx$gfrpt[forward] + xx$gsrpt[forward] # alternative path
xx$yptxb[forward] = xx$gfrptb[forward] + xx$gsrpt[forward] # current law
xx$ytpi[forward] = xx$gsrpri[forward] + xx$gfrpri[forward]  #production and import taxes
xx$grcsi[forward] = xx$gsrs[forward] + xx$gfrs[forward]  # payroll taxes
xx$yctlg[forward] = xx$gsrcp[forward] + xx$gfrcp[forward] # corporate taxes

### Calculate shares


