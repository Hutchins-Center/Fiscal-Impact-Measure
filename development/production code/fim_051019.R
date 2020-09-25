library(reshape2)
library(plyr)
library(zoo)
library(quantmod)
library(rmarkdown)
library(TTR)
library(Haver)
library(data.table)

# function to pull haver data 
pull_data = function(series, database, start.date, frequency = "quarterly"){
  q = haver.data(series, database, eop.dates = T, start = as.Date(start.date, f = "%m-%d-%Y"))
  q = data.frame(date = as.Date(rownames(q)), q)
  
  for (j in 2:ncol(q)) {
    for (k in 4:nrow(q)) {
      if (is.na(q[k,j])){
        q[k,j]=mean(q[c(k-1,k-2,k-3),j]) # if the data is missing on unreported, use the 3-qtr moving average

      }
    }
  }
  q
}

# function to calculate quarterly annualized growth rates
q_a = function(x){
  j=c()
  for(i in 2:length(x)){
    j[i] = (((x[i]/x[i-1])^4)-1)*100
  }
  j[1] = 0
  j
}

# function to calculate quarterly growth rate
q_g = function(x){
  j=c()
  for(i in 2:length(x)){
    j[i] = (((x[i]/x[i-1]))-1)
  }
  j[1] = 0
  j
}

series1 = c("GDP", "C","CH","GDPH","JC", "JGDP", "JGF", "JGS", "PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "GRCSI", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET")
data1 = pull_data(series1, "usna", start.date = "01-01-1970")
metadata1 = cbind(haver.metadata(series1, "usna")$code, haver.metadata(series1, "usna")$descriptor)
data2 = pull_data(c("GDPPOTHQ","GDPPOTQ", "RECESSQ"), "usecon", "01-01-1970")
hist = merge(data1, data2, by = "date")
  
# econ data
# data3 = pull_data(c("GDPBP","GDPHBP", "PGDPHBP", "PCECB",), "usecon", "01-01-1970")

# CBO's quarterly economic projections of GDP components
econ = read.csv('cbo_econ_proj_quarterly.csv', stringsAsFactors = F)
econ$date = gsub("12/30/", "12/31/", econ$date)
econ$date = as.Date(econ$date, f = "%m/%d/%Y")
comp = colnames(econ)[!colnames(econ) %in% "date"]
# econ[,comp] = lapply(econ[,comp], function(x) SMA(x, n=4))

# construct alternative scenario for federal g where discretionary caps do not expire
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

## CBO's annual budget projections, as they appear in the NIPAS
budg = read.csv('cbo_budget_nipas_proj_annual.csv', stringsAsFactors = F)
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

## FMAPS 
# come from CMS.gov, NHE by type of service and source of funds. Annual data, translated to quarterly shares just as we do with the budget data.  
fmap = read.csv('nhe_fmap.csv', stringsAsFactors = F)
fmap$fshare = fmap$gf_medicaid / (fmap$gf_medicaid + fmap$gs_medicaid)

## Regressions for forecasts of state and local tax revenues

# pull annual data for regressions for state and local tax revenues
series3 = c("GDP", "C","CH","GDPH","JC", "JGDP", "JGF", "JGS", "PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "GRCSI", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET", "YP")
series4 = c("USPHPI", "CASUSXAM", "FNMFHFA", "GDPPOT", "GDPPOTH")
data3 = pull_data(series3, "usna", start = as.Date("1970-01-01"), frequency = "annual")
data4 = pull_data(series4, "usecon", start = as.Date("1970-01-01"), frequency = "annual")
aa = merge(data3, data4, by = "date")
aa$hpx = aa$usphpi #house price index of choice is the FHFA purchase only index, 1991 = 100, since that's what CBO forecasts

econ_a = read.csv('cbo_econ_proj_annual.csv', stringsAsFactors = F)
econ_a$date = as.Date(paste0(econ_a$calendar_date, "-12-31"), f="%Y-%m-%d")
econ_a = econ_a[econ_a$date > Sys.Date(),] # keeping annuals for current calendar year
aa <- rbind.fill(aa,econ_a)

taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
taxpieces_d = paste0(taxpieces, "_d")
aa$gstx = rowSums(aa[,taxpieces], na.rm = T) # tax revenue total

# linear time trend var
aa$t = 1:nrow(aa)

# create lagged and differenced variables
comp = c("gdp", "gsrpri", "gsrcp" ,"gsrs","gstx", "hpx")
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
# try nominal levels first
forward_a = aa[aa$date > Sys.Date(),]
backward_a = aa[aa$date <= Sys.Date(),]

gstx_fit <- regs(y="gstx", x=c("t", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"), d=backward_a)
pieces_fit <- lapply(taxpieces, function(X){regs(y=X, x=c("t", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"), d=backward_a)})
aa[which(aa$date %in% forward_a$date),taxpieces] = lapply(pieces_fit, function(x) predict(x, forward_a)) ## FIX THIS
aa[which(aa$date %in% forward_a$date),taxpieces] = lapply(aa[which(aa$date %in% forward_a$date),taxpieces], function(x) sapply(x, function(y) max(y, 0))) ## FIX THIS

# try differences
# gstx_d_fit <- regs(y="gstx_d", x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"))
# pieces_d_fit <- lapply(taxpieces_d, function(X){regs(y=X, x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"))})


# translate into quarterly SAAR levels by replicating over four quarters and smoothing
aa = rbind(aa, aa, aa, aa) # we're going to use annual rates anyhow, so just replicate the annual levels over each q
aa = aa[order(aa$date),]
aa$date[format(aa$date, f="%Y") %in% format(econ$date, f="%Y")] = econ$date[format(econ$date, f="%Y") %in% format(aa$date, f="%Y")]

# smooth them out
aa[,taxpieces] = sapply(aa[,taxpieces], function(x) SMA(x, n=4))

# calculate growth rates
aa[,paste0(taxpieces, "_g")] = lapply(aa[,taxpieces], function(x) q_g(x))
aa = aa[(aa$date %in% econ$date),]

# Merge and Generate projections of GDP components
p = merge(budg[,c("date", grep("_g", colnames(budg), value = T))], econ[,c("date", grep("_g", colnames(econ), value = T))], by = "date", all = T)
xx = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "date", all = TRUE),list(aa[,c("date",grep("_g", colnames(aa), value = T))], hist, p))

xx$fshare = fmap$fshare[match(as.integer(format(as.Date(xx$date, format="%d-%m-%Y"),"%Y")), fmap$year)]
xx$fshare = na.locf(xx$fshare)

forward = which(!(xx$date %in% hist$date))

# clean up
rm(p)
rm(hist)

# special assumptions for projected growth rates
xx$gfrcf_g = xx$gfrcp_g # fed bank taxes grow with corporate taxes
# xx[,c(paste0(c("gsrpt", "gsrpri", "gsrs", "gstfp", "gsrcp"), "_g"))] = xx$gs_g # state and local taxes and transfers grow with state and local current expenditures
xx$gstfp_g = xx$gs_g # state and local transfers grow with state and local current expenditures
xx$gfeg_g = xx$gf_g # federal grants to state and local gov'ts grow with federal purchases
xx$jc_g = xx$jgf_g = xx$jgs_g = xx$jgdp_g # all government price indices grow with GDP price index. CHECK
xx$gfrptb = xx$gfrpt # preserve current law personal taxes, levels
xx$gfb = xx$gf # preserve current law federal expenditures, levels


# generate forward values of components using current levels and projected growth rates. 
comp = c("gdp", "c","yptmr" ,"yptmd" ,"g", "gfrcf", "gf", "gfeg", "gs", "gfrpt", "gfrpri","gfrcp" ,"gfrs", "gsrpt" ,"gsrpri", "gsrcp" ,"gsrs","gftfp", "gstfp", "gdppotq", "jgdp", "jc", "jgf", "jgs")
for(i in 1:length(comp)){
  for(j in 1:length(forward)){
    xx[forward[j], comp[i]] = xx[forward[j]-1, comp[i]]*(1+xx[forward[j], paste0(comp[i], "_g")])
  }
}

# assume FMAP federal share of medicaid expenditures remains constant.
xx$fshare[which(is.na(xx$fshare))] = xx$fshare[max(which(!is.na(xx$fshare)))]

# Additional component calculations
# Reattribute federal Medicaid grants to states back to Federal government based on FMAP shares (becuase of assumptions about FMAPs and federal grants, this reattribution doesn't matter for the forecasts at present.)
xx$gssave = xx$gs
xx$yfptmd = xx$yptmd*(xx$fshare) # federal medicaid expenditures
xx$gfegnet = xx$gfeg - xx$yfptmd # federal grants to state and local net of medicaid grants
xx$gf = xx$gf + xx$gfegnet # federal purchases = purchases +  grants net of medicaid 
xx$gs = xx$gs - xx$gfegnet # state and local purchases = purchases - grants net of medicaid 

# total tax and transfer pieces = state,local, plus federal tax and transfer pieces 
xx$gtfp[forward] = xx$gftfp[forward] + xx$gstfp[forward] # social benefits
xx$yptx[forward] = xx$gfrpt[forward] + xx$gsrpt[forward] # alternative path
xx$yptxb[forward] = xx$gfrptb[forward] + xx$gsrpt[forward] # current law
xx$ytpi[forward] = xx$gsrpri[forward] + xx$gfrpri[forward]  #production and import taxes
xx$grcsi[forward] = xx$gsrs[forward] + xx$gfrs[forward]  # payroll taxes
xx$yctlg[forward] = xx$gsrcp[forward] + xx$gfrcp[forward] # corporate taxes

          # addenda: deficits
          xx$deficit = -xx$gf - xx$gftfp + (xx$gfrpt + xx$gfrpri + xx$gfrcp + xx$gfrs) # alternative path
          xx$deficitb = -xx$gfb - xx$gftfp + (xx$gfrptb + xx$gfrpri + xx$gfrcp + xx$gfrs) # current law

##### FIM calculations ##### 

# Begin FIM calculations

fim = data.frame(
  date = xx$date,
  
  # taxes and transfers category totals
  health_outlays = xx$yptmr + xx$yptmd , # Medicare + Medicaid
  social_benefits = xx$gtfp - (xx$yptmr + xx$yptmd), # Social benefits net health outlays
  noncorporate_taxes = xx$yptx+xx$ytpi+xx$grcsi , # alternative
  corporate_taxes = xx$yctlg,  
  
  # consumption and investment totals
  federal_nom = xx$gf , 
  pi_federal = q_g(xx$jgf),

  state_local_nom = xx$gs ,
  pi_state_local = q_g(xx$jgs) ,

  
  # other needed reference variables 
  gdp = xx$gdp , # nominal gdp
  gdppoth = q_g(xx$gdppotq) - q_g(xx$jgdp), # real potential output growth
  pi_gdp = q_g(xx$jgdp), # gdp "deflator"
  pce = xx$c, # nominal consumption
  pi_pce = q_g(xx$jc))

fim = fim[fim$date < "2025-12-31",]

# Federal purchases contribution to real GDP growth
fim$federal_cont = NA
for(i in 2:nrow(fim)){
  fim$federal_cont[i] = 400*(fim$federal_nom[i] - (1 + fim$pi_federal[i] + fim$gdppoth[i])*fim$federal_nom[i-1])/fim$gdp[i-1]
}

# State and Local purchases contribution to real GDP growth
fim$state_local_cont = NA
for(i in 2:nrow(fim)){
  fim$state_local_cont[i] = 400*(fim$state_local_nom[i] - (1 + fim$pi_state_local[i] + fim$gdppoth[i])*fim$state_local_nom[i-1])/fim$gdp[i-1]
}

# Taxes and Transfers contribution to real GDP growth
# subtract counterfactual taxes and transfers from realized taxes
tts = c("health_outlays", "social_benefits", "noncorporate_taxes", "corporate_taxes")
fim[,tts] = lapply(fim[,tts], function(x){
  j = c()
  j[1] = x[1]
  for(i in 2:length(x)){
    j[i] = x[i] - x[i-1]*(1 + fim$pi_pce[i] + fim$gdppoth[i])
  }
  na.locf(j)
})


# Set MPC's - FYI
# mpc_health_outlays = c(0.9)
# mpc_social_benefits = c(0.9)
# mpc_noncorporate_taxes = c(-0.6,0.2,0.2,0.6)
# mpc_corporate_taxes = c(-0.4)

# Take category totals, net of counterfactual taxes, multiply by MPC's
fim$health_outlays_xmpc = 0.9*c(SMA(fim$health_outlays, n=4))
fim$social_benefits_xmpc = 0.9*c(SMA(fim$social_benefits, n=4))
fim$noncorporate_taxes_xmpc = NA
for(i in 8:nrow(fim)){
  if(is.na(fim$noncorporate_taxes[i-7])){
    fim$noncorporate_taxes_xmpc[i] = NA
  } else{
    lagstart = i-7
    lagend = i-2
    lags = lagstart:lagend
    fim$noncorporate_taxes_xmpc[i] = -0.6*(0.2*fim$noncorporate_taxes[i]+0.2*fim$noncorporate_taxes[i-1]+(0.6*mean(fim$noncorporate_taxes[lags])))
  }
}

fim$corporate_taxes_xmpc = NA
for(i in 12:nrow(fim)){
  if(is.na(fim$corporate_taxes[i-11])){
    fim$corporate_taxes_xmpc[i] = NA
  } else{
    lagstart = i-11
    lagend = i
    lags = lagstart:lagend
    fim$corporate_taxes_xmpc[i] = -0.4*mean(fim$corporate_taxes[lags]) 
  }
}


# Sum up transfers net taxes
fim$transfers_net_taxes = rowSums(fim[,grep("_xmpc", colnames(fim), value = T)], na.rm = T)

# calulate the taxes and transfers contribution to real GDP growth
fim$taxes_transfers_cont = NA
for(i in 2:nrow(fim)){
  fim$taxes_transfers_cont[i] = 400*fim$transfers_net_taxes[i] / fim$gdp[i-1]
}
fim = fim[1:(nrow(fim)-1),]
fim$fim_bars = rowSums(fim[,c("state_local_cont", "federal_cont", "taxes_transfers_cont")])
fim$fim_bars_ma = SMA(fim$fim_bars, n = 4)

fim = fim[fim$date >= "1980-01-01" & fim$date <= Sys.Date() + 2*367,]
fim_hist = fim[fim$date >= "2000-01-01" & fim$date <= Sys.Date(),]

write.csv(fim, paste0('fim-projections-', Sys.Date(), ".csv"))
write.csv(fim_hist, paste0('fim-history-', Sys.Date(), ".csv"))


# Plot figures
library(ggplot2)
library(gridExtra)
library(grid)
library(wesanderson)

uni.theme = theme_bw() + theme(legend.position = "bottom", legend.margin = unit(c(rep(-.8, 4)),"cm"), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank() , plot.margin=unit(c(1.2,.5,.5,.5),"cm") , plot.title = element_text(size=12), plot.subtitle = element_text(size=10) , legend.text=element_text(size=8), legend.title=element_blank()) 

colorz = c("black","#FD6467", "#00A08A",  "#5BBCD6", "#E6A0C4", wes_palette("Royal2")[3:5], wes_palette(n=5, "Darjeeling1"), wes_palette(n=5, "Royal2"), wes_palette(n=3, "Royal1"))

# recessions = data.frame(start = hist$date[which(diff(hist$Quarterly_NBER_Recession.Expansion__Recession_Shading_..1..1.)==2)], end = hist$date[which(diff(hist$Quarterly_NBER_Recession.Expansion__Recession_Shading_..1..1.)==-2)][-1])

shade = data.frame(start = as.Date(Sys.Date()), end = as.Date("2028-12-31",  "%Y-%m-%d"))

lp <- function(data, labelz = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = Sys.Date() + 2*367) {
  df_plot <- data
  colnames(df_plot) = c("date", labelz)
  df_plot <- melt(df_plot, id = "date")
  df_plot <- df_plot[which(df_plot$date <= end.date & df_plot$date >= start.date), ]
  
  leg_exists = NULL
  if(is.null(labelz)){
    leg_exists = theme(legend.position="none")
  }
  
  shade = data.frame(start = as.Date(Sys.Date()), end = as.Date(end.date,  "%Y-%m-%d"))
  
  ggplot() + geom_line(data = df_plot, aes_string("date", "value", colour = "variable")) +     
    uni.theme + 
    scale_x_date(breaks = 0, date_breaks = "4 years", date_labels = "%Y", limits = as.Date(c(start.date,end.date))) + 
    labs(title = t, subtitle = sub, caption = cap) + ylab(ylabel) + xlab("") +
    scale_colour_manual(values=colorz) + leg_exists + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.2)
}

bp <- function(data, labelz = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = "2022-12-31") {
  df_plot <- data
  colnames(df_plot) = c("date", labelz)
  df_plot <- melt(df_plot, id = "date")
  
  leg_exists = NULL
  if(is.null(labelz)){
    leg_exists = theme(legend.position="none")
  }
  
  ggplot() + geom_bar(data = df_plot, aes_string("date", "value", fill = "variable"), stat = "identity") + 
    uni.theme + 
    scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = "%Y", limits = as.Date(c(start.date,end.date))) + 
    labs(title = t, subtitle = sub, caption = cap) + ylab(ylabel) + xlab("") + 
    scale_fill_manual(values=colorz[c(5,4,3)]) + leg_exists  
  
}


fimbars = bp(data = fim[,c("date",c("state_local_cont", "federal_cont", "taxes_transfers_cont"))], labelz = c("State & Local", "Federal", "Taxes & Transfers"), start.date = "2000-01-01", t = "Hutchins Center Fiscal Impact Measure", sub = "Components of Fiscal Policy Contribution to Real GDP Growth") + geom_line(data = fim, aes(x = date, y = fim_bars_ma)) + geom_point(data = fim, aes(x = date, y = fim_bars_ma)) + geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection") + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.2) + ylim(-2,4)
fim$fim_barsmn = fim$fim_bars - fim$neutral_fi
fimcomps = bp(data = fim[,c("date","fim_bars")], labelz = c("Total Fiscal Impact"), start.date = "2000-01-01", t = "Hutchins Center Fiscal Impact Measure", sub = "Fiscal Policy Contribution to Real GDP Growth") + geom_line(data = fim, aes(x = date, y = fim_bars_ma)) + geom_point(data = fim, aes(x = date, y = fim_bars_ma)) + geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection") + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.2) + ylim(-2,4)


