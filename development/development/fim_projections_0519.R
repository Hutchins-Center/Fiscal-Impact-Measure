# pull data
source("fim_datapull.R")

# construct alternative scenario for federal purchases growth where discretionary caps do not expire, using CBO's quarterly economic projections
econ$gf_gdp = 100*econ$gf/econ$gdp
spendingmax = econ$date[which(econ$gf_gdp == max(econ$gf_gdp[econ$date >= "2018-01-01"]))] # first date of new caps
spendingmin = econ$date[which(econ$gf == min(econ$gf[econ$date >= spendingmax]))] # first date of spending growth
econ$spendingpath = c(NA,diff(econ$gf_gdp))
econ$spendingpath[which(econ$date <= spendingmax)] = 0
econ$spendingpath[which(econ$date >= spendingmax & econ$date <= spendingmin)] = mean(diff(econ$gf_gdp[econ$date > spendingmin]))

# alternative f to gdp ratio
econ$gf_gdpalt = econ$gf_gdp
altdates = which(econ$date >= spendingmax)
for(i in 2:length(altdates)){
  econ$gf_gdpalt[altdates[i]] = econ$gf_gdpalt[altdates[i-1]] + econ$spendingpath[altdates[i]]
}
econ$gf_gdpalt[altdates[length(altdates)]] = econ$gf_gdpalt[altdates[length(altdates) - 1]]

# alternative f level
econ$gfb = econ$gf # current law path 
econ$gfhb = econ$gfh # current law path, real
econ$dgf = econ$gf/econ$gfh # implied price deflator for federal expenditures (alternative is to use PCE deflator)
econ$gf[which(econ$date >= spendingmax)] =  (econ$gdp*econ$gf_gdpalt/100)[which(econ$date >= spendingmax)]
econ$gfh[which(econ$date >= spendingmax)] =  (econ$gdp*econ$gf_gdpalt/100)[which(econ$date >= spendingmax)] / econ$dgf[which(econ$date >= spendingmax)]

# calculate quarterly annualized growth rates
comp = c(comp,"gfb", "gfhb")
econ[,c(paste0(comp, "_g"))] = lapply(econ[,comp], function(x) q_g(x))

# take MA to smooth series where cbo adjusts to match fiscal year projections (?)
econ[,c(paste0(comp, "_g"))] = lapply(econ[,c(paste0(comp, "_g"))], function(x) SMA(x, n=4))


# construct forecasts of federal taxes and transfers growth using CBO's annual budget/revenue projections
budg = rbind(budg, budg, budg, budg) # we're going to use annual rates anyhow, so just replicate the annual levels over each q
budg = budg[order(budg$fy),]
budg$date = shift(econ$date[which(as.integer(format(as.Date(econ$date, format="%d-%m-%Y"),"%Y")) %in% budg$fy)], 1, type=c("lag")) # shift budget projection date (FY) to match calendar date 

# smooth them out
cc = c("gftfp",   "gfrpt",  "gfrpri",  "gfrcp",  "gfrs",  "gfcexp",   "gfsubs",   "gfintpmt", "yptmr",  "yptmd" )
budg[,cc] = sapply(budg[,cc], function(x) SMA(x, n=4))

# levels are already in annual terms, so do we want to take "q-o-q" growth rate 
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

taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
taxpieces_d = paste0(taxpieces, "_d")
aa$gstx = rowSums(aa[,taxpieces], na.rm = T) # tax revenue total

# linear time trend var
aa$t = 1:nrow(aa)

# create lagged and differenced variables
comp = c("gdp", "gsrpri", "gsrcp" ,"gsrs","gsrpt","gstx", "hpx")
aa[,paste0(comp, "_l1")] = sapply(aa[,comp], function(x) shift(x, n=1))
aa[,paste0(comp, "_l2")] = sapply(aa[,comp], function(x) shift(x, n=2))
aa[,paste0(comp, "_l3")] = sapply(aa[,comp], function(x) shift(x, n=3))
aa[,paste0(comp, "_l5")] = sapply(aa[,comp], function(x) shift(x, n=5))

diffs = paste0(comp, "_d")
aa[,diffs] = sapply(aa[,comp], function(x) c(NA, diff(x)))
aa[,paste0(diffs, "_l1")] = sapply(aa[,diffs], function(x) shift(x, n=1))
aa[,paste0(diffs, "_l2")] = sapply(aa[,diffs], function(x) shift(x, n=2))
aa[,paste0(diffs, "_l3")] = sapply(aa[,diffs], function(x) shift(x, n=3))
aa[,paste0(diffs, "_l5")] = sapply(aa[,diffs], function(x) shift(x, n=5))

# run regressions
regs <- function(y,x,d){
  spec = as.formula(paste0(y,"~",paste(x,collapse = "+")))
  lm(spec,d)
}

# preferred specification includes GDP and house prices and lags. For now, excluding own lags to avoid dynamic forecasts (they don't add much to the fit anyhow)
forward_aa = aa[aa$date > "2016-01-01",]
backward_a = aa[aa$date <= "2016-01-01",]
forward_dates = which(aa$date %in% forward_aa$date)

# try differences
gstx_d_fit <- regs(y="gstx_d", x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"), d=backward_a)
pieces_d_fit <- lapply(taxpieces_d, function(X){regs(y=X, x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"), d=backward_a)})
aa[forward_dates,paste0(taxpieces, "_d")] = lapply(pieces_d_fit, function(x) predict(x, forward_aa)) 
aa[forward_dates,taxpieces] = lapply(taxpieces, function(x){
  callname = paste0(x, "_d")
  j=c()
  j[1] = aa[forward_dates[1]-1,x] + aa[forward_dates[1],callname]
  for(i in 2:length(forward_dates)){
    j[i] = j[i-1] + aa[forward_dates[i],callname]
  }
  j
}) 

# translate into quarterly SAAR levels by replicating over four quarters and smoothing
aa = rbind(aa, aa, aa, aa) # we're going to use annual rates anyhow, so just replicate the annual levels over each q
aa = aa[order(aa$date),]
aa$date[format(aa$date, f="%Y") %in% format(econ$date, f="%Y")] = econ$date[format(econ$date, f="%Y") %in% format(aa$date, f="%Y")]

# smooth them out
aa[,taxpieces] = sapply(aa[,taxpieces], function(x) SMA(x, n=4))

# calculate growth rates
aa[,paste0(taxpieces, "_g")] = lapply(aa[,taxpieces], function(x) q_g(x))
aa = aa[(aa$date %in% econ$date),]

# Merge all projection dfs and generate projections of levels. We use projected growth rates and iteratively forecast levels from current levels. 
xx = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "date", all = TRUE),list(budg[,c("date", grep("_g", colnames(budg), value = T))], 
                                                                                 aa[,c("date",grep("_g", colnames(aa), value = T))],
                                                                                 econ[,c("date", grep("_g", colnames(econ), value = T))], 
                                                                                 hist))
forward = which(!(xx$date %in% hist$date))

# assume FMAP remains constant
xx$fshare = fmap$fshare[match(as.integer(format(as.Date(xx$date, format="%d-%m-%Y"),"%Y")), fmap$year)]
xx$fshare = na.locf(xx$fshare)


# assume FMAP federal share of medicaid expenditures remains constant.
xx$fshare[which(is.na(xx$fshare))] = xx$fshare[max(which(!is.na(xx$fshare)))]

# Additional component calculations
xx <- xx %>% mutate(
  
  # save some vars
  gfsave = gf, 
  gssave = gs, 
  gfb = gf, # preserve current law federal expenditures, levels
  gfrptb = gfrpt, # preserve current law personal taxes, levels
  
  # Reattribute federal non-Medicaid grants to states back to Federal government, using FMAP shares 
  yfptmd = yptmd*fshare, # federal medicaid expenditures
  ysptmd = yptmd*(1-fshare), # state medicaid payments = total medicaid * states' shares
  gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
  gf = gf + gfegnet, # federal purchases = purchases +  grants net of medicaid 
  gs = gs - gfegnet, # state and local purchases = purchases - grants net of medicaid 
  
  # Reattribute federal Medicaid grants to states back to federal government, away from state and local transfer payments
  gstfpnet = gstfp - yfptmd, # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
  gftfpnet = gftfp + yfptmd, # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
  
  # Make special assumptions for projected growth rates
  gstfp_g = gs_g,
  gstfpnet_g =  gs_g, # state and local transfers grow with state and local current expenditures
  gftfpnet_g = gftfp_g, # net federal transfers same as gross federal transfers
  yfptmd_g = yptmd_g, 
  ysptmd_g = yptmd_g, # disaggregated medicaid components grow with the aggregate
  gfeg_g = gf_g, # federal grants to state and local gov'ts grow with federal purchases
  jc_g = jgdp_g,
  jgf_g = jgdp_g,
  jgs_g = jgdp_g # all government price indices grow with GDP price index. CHECK
  
)

# generate forward values of components using current levels and projected growth rates. 
comp = c("gdph", "gdppothq", "gdp","c","yptmr","yptmd","g","gf","gfeg","gs","gfrpt","gfrpri","gfrcp","gfrs","gsrpt","gsrpri","gsrcp","gsrs","gftfp","gftfpnet","gstfp","gstfpnet","ysptmd","yfptmd","gdppotq","jgdp","jc","jgf","jgs")
for(i in 1:length(comp)){
  for(j in 1:length(forward)){
    xx[forward[j], comp[i]] = xx[forward[j]-1, comp[i]]*(1+xx[forward[j], paste0(comp[i], "_g")])
  }
}

# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
xx$gtfp[forward] = xx$gftfp[forward] + xx$gstfp[forward] # social benefits = federal benefits + state and local benefits
xx$yptx[forward] = xx$gfrpt[forward] + xx$gsrpt[forward] # alternative path
xx$yptxb[forward] = xx$gfrptb[forward] + xx$gsrpt[forward] # current law
xx$ytpi[forward] = xx$gsrpri[forward] + xx$gfrpri[forward]  #production and import taxes
xx$grcsi[forward] = xx$gsrs[forward] + xx$gfrs[forward]  # payroll taxes
xx$yctlg[forward] = xx$gsrcp[forward] + xx$gfrcp[forward] # corporate taxes


# addenda: deficits
xx <- xx %>% mutate(
  deficit = -gf - gftfp + (gfrpt + gfrpri + gfrcp + gfrs), # alternative path
  deficitb = -gfb - gftfp + (gfrptb + gfrpri + gfrcp + gfrs) # current law
)

