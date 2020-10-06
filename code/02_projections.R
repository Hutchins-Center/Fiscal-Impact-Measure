## ---------------------------
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
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# I prefer to view outputs in non-scientific notation
options(scipen = 6, digits = 4) 

## ---------------------------

# pull data
#source("code/fim_datapull.R")

last_proj_date = as.Date(paste0(year(Sys.Date()) + 2, "-12-31"))
last_hist_date = tail(hist$date, 1)


# Annual Budget Projections -----------------------------------------------------------------------------

# construct forecasts of federal taxes and transfers growth using CBO's annual budget/revenue projections 
# as they appear in the NIPAs (except Medicaid and Medicare, which come straight from revenue projections)

# we're going to use annual rates anyhow, so just replicate the annual levels over each q

budg <- 
  # we use annual rates, so we can just replicate annual levesl for each q
  bind_rows(budg, budg, budg, budg) %>% 
  arrange(fy) %>%
  mutate(date = econ$date) %>%
  mutate(date = lag(date))
 


# adjust federal transfers to feature their january COLA-related bump; 
# reattribute that growth to calendar quarter 1 before smoothing out the rest of the non-COLA related growth. 
#SSA uses CPI-W to create COLAs; we just take CBO's projection of CPI-U. That won't affect the level of total transfers, 
# just the timing a little bit

budg <- budg %>%
  mutate(cpiu = lag(econ$cpiu),
         cpiu_g = q_a(cpiu)/100,
         pcw = lag(hist$pcw),
         pcw_g = q_a(pcw)/100)
# Don't think this does anything  tbh
budg$pcw_g[is.na(budg$pcw_g)] = budg$cpiu_g[is.na(budg$pcw_g)]

# applicable cola rate is CPIW from Q3 of previous year

budg <- budg %>%
  mutate(cola_rate =
           case_when(
             month(date) == 3  ~ lag(cpiu_g, 2)
           )
         ) %>% 
  # forward filling so each of the quarters has its correct cola rate, 
  # remove negative value
  fill(cola_rate)



# COLA Adjustments --------------------------------------------------------------------------------------

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



# Alternate tax scenario --------------------------------------------------------------------------------

# construct alternative scenario for personal current taxes, 
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

# construct forecasts of state and local taxes growth
aa <- plyr::rbind.fill(aa,econ_a)
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


# Implicit price deflators ------------------------------------------------------------------------------

# calculate CBO's implicit price deflators for gf, gs, c
# growth rate of nominal over real
econ[,paste0("j",c("gf", "gs", "c"))] = lapply(c("gf", "gs", "c"), function(x){
  econ[,x]/econ[,paste0(x,"h")]
})

# Quarterly rates ---------------------------------------------------------------------------------------

econ[,c(paste0(comp, "_g"))] = lapply(econ[,comp], function(x) q_g(x))
econ[,paste0("j",c("gf", "gs", "c"), "_g")] = lapply(econ[,paste0("j",c("gf", "gs", "c"))], function(x) q_g(x))

econ <- merge(aa[,c("date", taxpieces, taxpieces_gdp)], econ, by = "date", all.x = F)
econ[,taxpieces] = sapply(econ[,taxpieces_gdp], function(x) x*econ$gdp)


# calculate growth rates
econ[,paste0(taxpieces, "_g")] = lapply(econ[,taxpieces], function(x) q_g(x))

# Merge all projection dfs and generate projections of levels. 
# We use projected growth rates and iteratively forecast levels from current levels. 
xx = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "date", all = TRUE),
            list(budg[,c("date", grep("_g", colnames(budg), value = T))], 
                 # aa[,c("date",grep("_g", colnames(aa), value = T))],
                  econ[,c("date", grep("_g", colnames(econ), value = T))], 
                  hist))

forward = which(!(xx$date %in% hist$date))

#reattribute state unemployment from federal back to state
#includes federal UI which we need to take out 768.8 [legislation total] from Q2 of state unemployment insurance
xx$gftfbusx = xx$gftfbusx/1000 #translate from millions to billions
xx$gftfp = xx$gftfp - xx$gftfbusx # + 768.8
xx$gftfp[202] = xx$gftfp[202] + 768.8

xx$gstfp = xx$gstfp + xx$gftfbusx # - 768.8
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
  # past cap expiration dates, CBO assumes that fed purchases just grow with inflation. 
  # we want to assume they grow with nominal potential (zero impact, essentially)
  
  gf_g = ifelse((date > "2021-09-30"), gdppothq_g + jgdp_g, gf_g), 
  gstfp_g = gs_g, # state and local transfers grow with state and local current expenditures
  gstfpnet_g =  gs_g, # state and local transfers grow with state and local current expenditures
  gftfpnet_g = gftfp_g, # net federal transfers same as gross federal transfers

  gfeghhx_g = yptmd_g, # federal health grants to states grow with medicaid
  gfeghdx_g = yptmd_g, # federal medicaid grants to states grow with medicaid
  
  gfeg_g = gf_g, # federal current grants to state and local gov'ts grow with federal purchases
  gfeigx_g = gf_g, # federal capital grants to state and local gov'ts grow with federal purchases
  
  jgsi_g = jgs_g,
  jgse_g = jgs_g, # deflators for state & local investment, consumption grow with overall deflator
  
  gfsub_g = gdppothq_g,
  gssub_g = gdppothq_g # subsidies grow with potential
  
  # yfptmd_g = yptmd_g,  # disaggregated medicaid components grow with the aggregate
  # ysptmd_g = yptmd_g, # disaggregated medicaid components grow with the aggregate
)

# TEMPORARY ADJUSTMENT TO GROWTH RATES FOR FEDERAL GRANTS
# xx$gfeg_g[xx$date == "2020-03-31"] = xx$gfeg_g[xx$date == "2019-12-31"][1]
# xx$gfeigx_g[xx$date == "2020-03-31"] = xx$gfeg_g[xx$date == "2019-12-31"][1]

# generate forward values of components using current levels and projected growth rates. 
comp2 = c("gdph", "gdppothq", "gdp","c", "yptmr","yptmd","g","gf", 
          "gfeg","gfeigx","gfeghhx", "gfeghdx", "gs", "gfrpt","gfrpri","gfrcp",
          "gfrs","gsrpt","gsrpri","gsrcp","gsrs","gftfp","gstfp","gdppotq",
          "jgdp","jc","jgf","jgs","jgse", "jgsi", "gssub", "gfsub")
for(i in 1:length(comp2)){
  for(j in 1:length(forward)){
    xx[forward[j], comp2[i]] = xx[forward[j]-1, comp2[i]]*(1+xx[forward[j], paste0(comp2[i], "_g")])
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
                  gfeghdx), # otherwise, use data for medicaid + prescription drugs transfers
  
  # state medicaid payments = total medicaid - federal medicaid grants
  ysptmd = yptmd - yfptmd,
  gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
  
  # Reattribute federal Medicaid grants to states back to federal government, away from state and local transfer payments
  # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
  gstfpnet = gstfp - yfptmd, 
  # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
  gftfpnet = gftfp + yfptmd 
  # we reattribute the capital grants later after calculating contributions. 
  
) 

# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
xx$gtfp[forward] = xx$gftfp[forward] + xx$gstfp[forward] # social benefits = federal benefits + state and local benefits
xx$yptx[forward] = xx$gfrpt[forward] + xx$gsrpt[forward] # alternative path
xx$yptxb[forward] = xx$gfrptb[forward] + xx$gsrpt[forward] # current law
xx$ytpi[forward] = xx$gsrpri[forward] + xx$gfrpri[forward]  #production and import taxes
xx$grcsi[forward] = xx$gsrs[forward] + xx$gfrs[forward]  # payroll taxes
xx$yctlg[forward] = xx$gsrcp[forward] + xx$gfrcp[forward] # corporate taxes
xx$gsub[forward] = xx$gssub[forward] + xx$gfsub[forward] # subsidies

