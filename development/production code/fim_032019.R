rm(list=ls())
library(reshape2)
library(plyr)
library(zoo)
library(quantmod)
library(rmarkdown)
library(TTR)
library(Haver)
library(data.table)

setwd('C:/Users/sbelz/The Brookings Institution/Hutchins Center Team - Documents/Projects/Fiscal Impact/Forecasts')

# function to pull haver data 
pull_data = function(series, database, start.date){
  q = haver.data(series, database, eop.dates = T, start = as.Date(start.date, f = "%m-%d-%Y"))
  q = data.frame(date = as.Date(rownames(q)), q)
  
  for (j in 2:ncol(q)) {
    for (k in 2:nrow(q)) {
      if (is.na(q[k,j])){
        q[k,j]=q[k-1,j]
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

data1 = pull_data(c("GDP", "C","CH","GDPH","JC","PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "grcsi", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET"), "usna", start.date = "01-01-1970")
metadata1 = cbind(haver.metadata(c("GDP", "C","CH","GDPH","JC","PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "grcsi", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET"), "usna")$code, haver.metadata(c("GDP", "C","CH","GDPH","JC","PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "grcsi", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET"), "usna")$descriptor)

data2 = pull_data(c("GDPPOTHQ","RECESSQ"), "usecon", "01-01-1970")

history = merge(data1, data2, by = "date")
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
spendingmin = econ$date[which(econ$gf == min(econ$gf[econ$date >= spendingmax]))]# first date of spending growth
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
econ[,c(paste0(comp, "_g"))] = c(lapply(econ[,comp], function(x) q_a(x)))

# take MA to smooth series where cbo adjusts to match fiscal year projections (?)
econ[,c(paste0(comp, "_g"))] = lapply(econ[,c(paste0(comp, "_g"))], function(x) SMA(x, n=4))

## CBO's annual budget projections, as they appear in the NIPAS
budg = read.csv('cbo_budget_nipas_proj_annual.csv', stringsAsFactors = F)

budg = rbind(budg, budg, budg, budg) # we're going to use annual rates anyhow, so just replicate the annual levels over each q
budg = budg[order(budg$fy),]
budg$date = shift(econ$date[which(as.integer(format(as.Date(econ$date, format="%d-%m-%Y"),"%Y")) %in% budg$fy)],1, type=c("lag")) # shift budget projection date (FY) to match calendar date 

# smooth them out
cc = c("gftfp",   "gfrpt",  "gfrpri",  "gfrcp",  "gfrs",  "gfcexp",   "gfsubs",   "gfintpmt", "yptmr",  "yptmd" )
budg[,cc] = sapply(budg[,cc], function(x) SMA(x, n=4))

# levels are already in annual terms, so do we want to take "q-o-q" growth rate (??)
budg[,c(paste0(cc, "_g"))] = sapply(budg[,cc], function(x) q_a(x))


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



# Generate projections of GDP components
p = merge(budg[,c("date", grep("_g", colnames(budg), value = T))], econ[,c("date", grep("_g", colnames(econ), value = T))], by = "date")
xx = merge(history, p, by = "date", all = T)
forward = which(!(xx$date %in% history$date))

xx$gfrptb = xx$gfrpt # current law personal taxes
xx$gfb = xx$gf # current law federal expenditures

# special assumptions for growth rates
xx$gfrcf_g = xx$gfrcp_g # fed bank taxes grow with corporate taxes
xx[,c(paste0(c("gsrpt", "gsrpri", "gsrs", "gstfp", "gsrcp"), "_g"))] = xx$gs_g # state and local taxes and transfers grow with state and local current expenditures

comp = c("gdp", "c",   "ch",  "gdph","yptmr" ,"yptmd" ,"g", "gfrcf",  "gf", "gfb", "gs", "gfh", "gsh", "gfrpt","gfrptb", "gfrpri" ,"gfrcp" ,"gfrs", "gftfp", "gsrpt" ,"gsrpri", "gsrcp" ,"gsrs", "gstfp", "gdppothq", "dc")
for(i in 1:length(comp)){
  for(j in 1:length(forward)){
    xx[forward[j], comp[i]] = xx[forward[j]-1, comp[i]]*(1+xx[forward[j], paste0(comp[i], "_g")]/400)
  }
}



# xx$dc[forward] = p$dc[which(p$date %in% xx$date[forward])]

# total tax and transfer pieces = state,local, plus federal tax and transfer pieces 
xx$gtfp[forward] = xx$gftfp[forward] + xx$gstfp[forward]
xx$yptx[forward] = xx$gfrpt[forward] + xx$gsrpt[forward] # alternative path
xx$yptxb[forward] = xx$gfrptb[forward] + xx$gsrpt[forward] # current law
xx$ytpi[forward] = xx$gsrpri[forward] + xx$gfrpri[forward]  
xx$grcsi[forward] = xx$gsrs[forward] + xx$gfrs[forward]  
xx$yctlg[forward] = xx$gsrcp[forward] + xx$gfrcp[forward]

# addenda: deficits
xx$deficit = -xx$gf - xx$gftfp + (xx$gfrpt + xx$gfrpri + xx$gfrcp + xx$gfrs) # alternative path
xx$deficitb = -xx$gfb - xx$gftfp + (xx$gfrptb + xx$gfrpri + xx$gfrcp + xx$gfrs) # current law

 
# calculate shares of gdp
shares = c( "c","yptmr" ,"yptmd" ,"g", "gfrcf",  "gf", "gfb", "gs", "gfrpt","gfrptb", "gfrpri" ,"gfrcp" ,"gfrs", "gftfp", "gsrpt" ,"gsrpri", "gsrcp" ,"gsrs", "gstfp", "gtfp", "yptx", "yptxb", "ytpi", "grcsi", "yctlg", "deficit", "deficitb")
xx[, paste0(shares, "_gdp")] = lapply(xx[, shares], function(x) 100*x/xx$gdp)

##### FIM calculations ##### 

# Set MPC's
mpc_health_outlays = c(0.9)
mpc_social_benefits = c(0.9)
mpc_noncorporate_taxes = c(-0.6,0.2,0.2,0.6)
mpc_corporate_taxes = c(-0.4)

# Begin calculations

fim = data.frame(
  date = xx$date,
  
  # taxes and transfers category totals
  health_outlays = xx$yptmr + xx$yptmd , # Medicare + Medicaid
  social_benefits = xx$gtfp - (xx$yptmr + xx$yptmd), # Social benefits net health outlays
  noncorporate_taxes = xx$yptx+xx$ytpi+xx$grcsi , # alternative
  noncorporate_taxesb = xx$yptxb+xx$ytpi+xx$grcsi , # current law
  corporate_taxes = xx$yctlg,  #- xx$gfrcf, # net receipts from fed banks
  
  # government consumption and investment totals
  federal_real = xx$gfh ,
  federal_realb = xx$gfh , # current law
  federal_real_growth = xx$gfh_g ,
  federal_real_growthb = xx$gfhb_g , # current law
  federal_nom = xx$gf , 
  federal_nomb = xx$gfb ,  # current law
  federal_cont = xx$ptgfh, 
  
  state_local_real = xx$gsh ,
  state_local_real_growth = xx$gsh_g ,
  state_local_nom = xx$gs ,
  state_local_cont = xx$ptgsh ,
  
  nom_federal_share = (xx$gf) / xx$gdp ,
  nom_federal_shareb = (xx$gfb) / xx$gdp , # current law
  nom_state_local_share = (xx$gs) / xx$gdp ,
  nom_g_share = (xx$gs + xx$gf) / xx$gdp ,
  
  
  # other needed reference variables 
  real_gdp = xx$gdph ,
  nom_gdp = xx$gdp ,
  growth_real_gdp = q_a(xx$gdph),
  growth_potential_real_gdp = q_a(xx$gdppothq),
  
  nom_consumption_share = (xx$c / xx$gdp) ,
  pce_deflator = xx$dc ,
  real_pce = xx$ch ,
  growth_real_pce = q_a(xx$ch))

# Federal Component
fim$federal_cont[forward] = fim$nom_federal_share[forward] * fim$federal_real_growth[forward]
fim$federal_contb[forward] = fim$nom_federal_shareb[forward] * fim$federal_real_growthb[forward] # current law

# State and Local Component
fim$state_local_cont[forward] = fim$nom_state_local_share[forward] * fim$state_local_real_growth[forward]

# Taxes and Transfers component 
# category totals * MPC's
fim$health_outlays_xmpc = mpc_health_outlays*c(SMA(fim$health_outlays, n=4))
fim$social_benefits_xmpc = mpc_social_benefits*c(SMA(fim$social_benefits, n=4))
fim$noncorporate_taxes_xmpc = NA
for(i in 8:nrow(fim)){
  if(is.na(fim$noncorporate_taxes[i-7])){
    fim$noncorporate_taxes_xmpc[i] = NA
  } else{
    lagstart = i-7
    lagend = i-2
    lags = lagstart:lagend
    fim$noncorporate_taxes_xmpc[i] = mpc_noncorporate_taxes[1]*(mpc_noncorporate_taxes[2]*fim$noncorporate_taxes[i]+mpc_noncorporate_taxes[3]*fim$noncorporate_taxes[i-1]+(mpc_noncorporate_taxes[4]*mean(fim$noncorporate_taxes[lags])))
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
    fim$corporate_taxes_xmpc[i] = mpc_corporate_taxes[1]*mean(fim$corporate_taxes[lags]) 
    
  }
}

# fim$corporate_taxes_xmpc = mpc_corporate_taxes*c(SMA(fim$corporate_taxes, n=12))


# deflate by PCE deflator
fim$outlays_net_taxes = rowSums(fim[,grep("_xmpc", colnames(fim), value = T)], na.rm = T)
# fim$outlays_net_taxesb = rowSums(fim[,c("health_outlays_xmpc","social_benefits_xmpc","noncorporate_taxesb_xmpc","corporate_taxes_xmpc")], na.rm = T)

fim$real_outlays_net_taxes = 100*fim$outlays_net_taxes / fim$pce_deflator
# fim$real_outlays_net_taxesb = 100*fim$outlays_net_taxesb / fim$pce_deflator

fim$consumption_no_taxes = fim$real_pce - fim$real_outlays_net_taxes
# fim$consumption_no_taxesb = fim$real_pce - fim$real_outlays_net_taxesb

# calulate growth rates
fim$growth_consumption_no_taxes = q_a(fim$consumption_no_taxes)
# fim$growth_consumption_no_taxesb = q_a(fim$consumption_no_taxesb)

fim$difference_in_consumption_growth = fim$growth_real_pce - fim$growth_consumption_no_taxes
# fim$difference_in_consumption_growthb = fim$growth_real_pce - fim$growth_consumption_no_taxesb

# calulate the taxes and transfers contribution to real GDP growth
fim$taxes_transfers_cont = fim$nom_consumption_share * fim$difference_in_consumption_growth
# fim$taxes_transfers_contb = fim$nom_consumption_share * fim$difference_in_consumption_growthb

# calculate total FI and neutral FI
fim$neutral_fi = fim$nom_g_share * fim$growth_potential_real_gdp
fim$fim_bars = rowSums(fim[,c("state_local_cont", "federal_cont", "taxes_transfers_cont")])
fim$fim_bars_ma = SMA(fim$fim_bars, n = 4)

fim = fim[fim$date >= "1980-01-01" & fim$date <= Sys.Date() + 2*367,]

write.csv(fim, paste0('fim-projections-', Sys.Date(), ".csv"))


# Plot figures
library(ggplot2)
library(gridExtra)
library(grid)
library(wesanderson)

uni.theme = theme_bw() + theme(legend.position = "bottom", legend.margin = unit(c(rep(-.8, 4)),"cm"), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank() , plot.margin=unit(c(1.2,.5,.5,.5),"cm") , plot.title = element_text(size=12), plot.subtitle = element_text(size=10) , legend.text=element_text(size=10), legend.title=element_blank()) 

colorz = c("black","#FD6467", "#00A08A",  "#5BBCD6", "#E6A0C4", wes_palette("Royal2")[3:5], wes_palette(n=5, "Darjeeling1"), wes_palette(n=5, "Royal2"), wes_palette(n=3, "Royal1"))

# recessions = data.frame(start = history$date[which(diff(history$Quarterly_NBER_Recession.Expansion__Recession_Shading_..1..1.)==2)], end = history$date[which(diff(history$Quarterly_NBER_Recession.Expansion__Recession_Shading_..1..1.)==-2)][-1])

shade = data.frame(start = as.Date(Sys.Date()), end = as.Date("2028-12-31",  "%Y-%m-%d"))

lp <- function(data, labels = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = Sys.Date() + 2*367) {
  df_plot <- data
  colnames(df_plot) = c("date", labels)
  df_plot <- melt(df_plot, id = "date")
  df_plot <- df_plot[which(df_plot$date <= end.date & df_plot$date >= start.date), ]
  
  leg_exists = NULL
  if(is.null(labels)){
    leg_exists = theme(legend.position="none")
  }
  
  shade = data.frame(start = as.Date(Sys.Date()), end = as.Date(end.date,  "%Y-%m-%d"))
  
  ggplot() + geom_line(data = df_plot, aes_string("date", "value", colour = "variable")) +     
    uni.theme + 
    scale_x_date(breaks = 0, date_breaks = "4 years", date_labels = "%Y", limits = as.Date(c(start.date,end.date))) + 
    labs(title = t, subtitle = sub, caption = cap) + ylab(ylabel) + xlab("") +
    scale_colour_manual(values=colorz) + leg_exists + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.2)
}

bp <- function(data, labels = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = "2022-12-31") {
  df_plot <- data
  colnames(df_plot) = c("date", labels)
  df_plot <- melt(df_plot, id = "date")
  
  leg_exists = NULL
  if(is.null(labels)){
    leg_exists = theme(legend.position="none")
  }
  
  ggplot() + geom_bar(data = df_plot, aes_string("date", "value", fill = "variable"), stat = "identity") + 
    uni.theme + 
    scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = "%Y", limits = as.Date(c(start.date,end.date))) + 
    labs(title = t, subtitle = sub, caption = cap) + ylab(ylabel) + xlab("") + 
    scale_fill_manual(values=colorz[c(5,4,3)]) + leg_exists  
  
}




fimbars = bp(data = fim[,c("date",c("state_local_cont", "federal_cont", "taxes_transfers_cont"))], labels = c("State & Local", "Federal", "Taxes & Transfers"), start.date = "2000-01-01", t = "Hutchins Center Fiscal Impact Measure", sub = "Components of Fiscal Policy Contribution to Real GDP Growth") + geom_line(data = fim, aes(x = date, y = fim_bars_ma)) + geom_point(data = fim, aes(x = date, y = fim_bars_ma)) + geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection") + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.2) + ylim(-2,4)
fim$fim_barsmn = fim$fim_bars - fim$neutral_fi
fimcomps = bp(data = fim[,c("date","fim_bars")], labels = c("Total Fiscal Impact"), start.date = "2000-01-01", t = "Hutchins Center Fiscal Impact Measure", sub = "Fiscal Policy Contribution to Real GDP Growth") + geom_line(data = fim, aes(x = date, y = fim_bars_ma)) + geom_point(data = fim, aes(x = date, y = fim_bars_ma)) + geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection") + geom_rect(data = shade, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.2) + ylim(-2,4)




gfpct <- lp(data = xx[,c("date", "gfb_gdp","gf_gdp")], c("Current law", "Alternative: Discretionary caps do not expire"), t = "Federal consumption & invesment, current law and alternative", sub = "Share of GDP (%)")
gflev <- lp(xx[,c("date", "gfb","gf")], c("Current law", "Alternative: Discretionary caps do not expire"), t = "Federal consumption & invesment, current law and alternative", sub = "Billions ($)")
gpct <- lp(xx[,c("date", "gs_gdp", "gf_gdp")], c("State & Local", "Federal*"), t = "Government consumption & invesment", sub = "Share of GDP (%)", c = "*Alternative discretionary spending")
glev <- lp(xx[,c("date", "gs", "gf")], c("State & Local", "Federal*"), t = "Government consumption & invesment", sub = "Billions ($)", c = "*Alternative discretionary spending")
gtfpct <- lp(xx[,c("date", "gstfp_gdp", "gftfp_gdp")], c("State & Local", "Federal"), t = "Government benefits to persons", sub = "Share of GDP (%)", c = "Includes Medicare and Medicaid")
gtflev <- lp(xx[,c("date", "gstfp", "gftfp")], c("State & Local", "Federal"), t = "Government benefits to persons", sub = "Billions ($)", c = "Includes Medicare and Medicaid")
yptpct <- lp(xx[,c("date", "yptmr_gdp", "yptmd_gdp")], c("Medicare", "Medicaid"), t = "Health Spending", sub = "Share of GDP (%)")
yptlev <- lp(xx[,c("date", "yptmr", "yptmd")], c("Medicare", "Medicaid"), t = "Health Spending", sub = "Billions ($)")
ytlev <- lp(xx[,c("date", "yptx", "ytpi", "yctlg", "grcsi")], c("Personal*", "Production & Import", "Corporate", "Payroll"), t = "Current taxes", sub = "Billions ($)", c = "*Alternative assumption (no sunset on TCJA provisions)")
ytpct <- lp(xx[,c("date", "yptx_gdp", "ytpi_gdp", "yctlg_gdp", "grcsi_gdp")], c("Personal*", "Production & Import", "Corporate", "Payroll"), t = "Current taxes", sub = "Share of GDP (%)", c = "*Alternative assumption (no sunset on TCJA provisions)") + guides(fill=guide_legend(nrow=2,byrow=TRUE))
yptxlev <- lp(xx[,c("date", "yptx", "yptxb")], c("Alternative: No sunset", "Current law"), t = "Personal Current Taxes under current law and alternative assumptions, federal", sub = "Billions ($)")
yptxpct <- lp(xx[,c("date", "yptx_gdp", "yptxb_gdp")], c("Alternative: No sunset", "Current law"), t = "Personal Current Taxes", sub = "Share of GDP (%)")

pdf(paste0("component-projections-",Sys.Date(),".PDF"), height = 8.5, width = 11)
grid.arrange(fimbars, fimcomps,  nrow = 2, top = textGrob("FIM Projections", gp=gpar(fontsize=15,font=2)), heights = c(1.5,1.5))
grid.arrange(gflev, gfpct, glev, gpct,  nrow = 2, ncol = 2, top = textGrob("Government consumption & invesment, actual and projected, 2000-2028", gp=gpar(fontsize=15,font=2)), heights = c(1,1))
grid.arrange(gtflev, gtfpct, yptlev, yptpct,   nrow = 2, ncol = 2, top = textGrob("Government benefits, actual and projected, 2000-2028", gp=gpar(fontsize=15,font=2)))
grid.arrange(ytlev, ytpct, yptxlev, yptxpct,  nrow = 2, ncol = 2, top = textGrob("Taxes, actual and projected, 2000-2028", gp=gpar(fontsize=15,font=2, vjust = -10)))
# grid.arrange(ytlev, ytpct, yptxlev, yptxpct,  nrow = 2, ncol = 2, top = textGrob("Addenda", gp=gpar(fontsize=15,font=2, vjust = -10)))
dev.off()


lp(xx[,c("date", "deficit_gdp", "deficitb_gdp")], c("alternative", "current law"))

# "Total" =  c(0.7, 3.6, 1.1, 0.4, 0.0, 0.5), 
annualproj = data.frame(Federal = c(1.0, 6.9, 0.9, -0.5, -1.4, 0.3), "State_local" = c(0.5, 1.6, 1.3, 1.0, 0.8, 0.6), yr = c("Actual, 2017", "2018", "2019", "2020", "2021-2022", " 2023-2028"))
annualproj = melt(annualproj, id = "yr")
annualproj = annualproj[order(annualproj$yr),]
ggplot(annualproj, aes(x=yr, y=value, group = variable)) + geom_line(aes(colour = variable)) +  uni.theme + ylab("Percent") +scale_colour_manual(values=colorz)
