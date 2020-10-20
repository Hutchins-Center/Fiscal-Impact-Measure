rm(list=ls())
library(reshape2)
library(plyr)
library(quantmod)
library(ggplot2)
library(data.table)
library(fBasics)
library(wesanderson)
library(dLagM)
library(broom)
library(stargazer)
library(vars)

################
#The code below computes the Hutchins Center's Fiscal Impact Measure according to new methodology as of April 2019. The methodology adjusts the composition of overall purchases to account for non-medicaid Federal Grants (re-apportioning it from State to Federal purchases), adjusts the baseline growth rate (to potential output grwoth), and adds numerous sub-categories for Federal and State and Automatic and Discretionary components of the FIM. 


#---------#
#Read data#
#---------#

if (Sys.info()[6]=="mng"){
  path="C:/Users/mng/OneDrive - The Brookings Institution/hutchins/FIM/FIM_new"
} else { 
  path=paste0(paste0("C:/Users/",Sys.info()[6]),"/The Brookings Institution/FIM_new")
}


haver=read.csv(paste0(path,'/HaverData.csv'),stringsAsFactors=FALSE)
haver$date=as.Date(haver$date,format="%m/%d/%Y")
haver=arrange(haver,date)

for (i in 2:ncol(haver)){
  haver[,i]=as.numeric(gsub("%","",haver[,i]))
  haver[,i]=as.numeric(gsub("#N/A",NA,haver[,i]))
}

haver$ugap=haver$unemp-haver$NAIRU

#Quarterly data for Emergency Unemployment Compensation is obtained from the BLS. We use annual data prior to 2008 and the quarterly data, pulled manually, for the Great Recession <https://oui.doleta.gov/unemploy/euc.asp>
haver$euc=haver$euc*4 #annualizing

#FMAP Data on Medicaid shares between Federal and S&L is updated annually from CMS and interpolated forward <https://www.cms.gov/research-statistics-data-and-systems/statistics-trends-and-reports/nationalhealthexpenddata/nationalhealthaccountshistorical.html>. Temporary expansions in the federal share of medicaid were made during the Great Recession for which we make adjustments manually (see )
haver$federal.medicaid.share=as.numeric(as.vector(na.locf(haver$federal.medicaid.share)))

haver$Nominal.G.Federal.exdef=haver$Nominal.G.Federal-haver$Nominal.G.Federal.Defense

#starting in Q2 2018, the BEA re-classified Fed remittances as corporate dividends rather than corporate taxes and retoractively netted it out of Corporate Taxes. It is included here but the NIPAs no longer include remittances in corproate taxes
haver$Corporate.Taxes.from.Fed.Banks[haver$date>="2018-04-01"]=0 

haver.a=read.csv(paste0(path,'/HaverData.a.csv'),stringsAsFactors=FALSE)
haver.a$date=as.Date(haver.a$date,format="%m/%d/%Y")
haver.a=arrange(haver.a,date)

for (i in 2:ncol(haver.a)){
  haver.a[,i]=as.numeric(gsub("%","",haver.a[,i]))
  haver.a[,i]=as.numeric(gsub("#N/A",NA,haver.a[,i]))
}

date.q=as.data.frame(haver$date)
colnames(date.q)="date"
haver.a=merge(haver.a,date.q,by="date",all=TRUE)
haver.a=arrange(haver.a,date)
haver.a=na.locf(haver.a)
for (i in 2:ncol(haver.a)){
  haver.a[,i]=as.numeric(haver.a[,i])
}
haver.a$date=as.Date(haver.a$date)
colnames(haver.a)[-1]=paste0(colnames(haver.a)[-1],".a")
haver=merge(haver,haver.a,by="date",all=TRUE)

#use annual data on Emergency Unemployment Compensation prior to 2008
haver$euc[haver$date<"2008-01-01"]=haver$Emergency.Unemployment.Compensation.a[haver$date<"2008-01-01"]
haver$euc[is.na(haver$euc)]=0

#if the data is missing on unreported, use the 3-qtr moving average from the last period

lag.data=c("Corporate.Taxes","State.Corporate.taxes","Federal.Corporate.Taxes","cbo.stabilizers.outlays","cbo.stabilizers.revenue")
for (j in 1:length(lag.data)){
haver[,paste0(lag.data[j],".ma")]=rollapply(haver[,lag.data[j]],width=3,FUN=function(y) mean(y,na.rm=T),fill=NA,by.column=FALSE,align="right")
for (i in 2:nrow(haver)){
  if (haver[i,lag.data[j]]==0 | is.na(haver[i,lag.data[j]]==0)){
    haver[i,lag.data[j]]=haver[i-1,paste0(lag.data[j],".ma")]
  }
  }
}


#------------------------#
#Compute overall metrics #
#------------------------#

#reattribute federal Medicaid grants to states back to Federal government based on FMAP shares
haver$State.Medicaid.adj=(1-haver$federal.medicaid.share.adj)*haver$Medicaid.Spending
haver$non.medicaid.grants=haver$Federal.Grants-haver$federal.medicaid.share.adj*haver$Medicaid.Spending
haver$non.medicaid.grants.gdp=haver$non.medicaid.grants/haver$Nominal.GDP
haver$federal.medicaid=haver$Medicaid.Spending-haver$State.Medicaid.adj

#Re-attribute all non-medicaid grants from state purchases to federal purchases
haver$new.federal=haver$Nominal.G.Federal+haver$non.medicaid.grants
haver$new.federal.exdef=haver$Nominal.G.Federal.exdef+haver$non.medicaid.grants
haver$new.state=haver$Nominal.G.Federal+haver$Nominal.G.State-haver$new.federal

#Social Benefits excluding Health
haver$social.ex.health=haver$Social.Benefits.a-haver$Federal.Medicare.a-haver$State.Medicaid.a
haver$federal.social.ex.medicare=haver$Federal.Social.Benefits.a-haver$Federal.Medicare.a
haver$state.social.ex.medicaid=haver$State.Social.Benefits.a-haver$State.Medicaid.a
haver$federal.social.ex.health.share=haver$federal.social.ex.medicare/haver$social.ex.health

#Tax Rates
haver$federal.corp.tax.rate=haver$Federal.Corporate.Taxes/haver$Nominal.GDP
haver$state.corp.tax.rate=haver$State.Corporate.taxes/haver$Nominal.GDP
haver$state.ind.tax.rate=(haver$State.Personal.Current+haver$State.Taxes.on.production+haver$State.Contributions.for.social.insurance)/haver$Nominal.GDP

#---------------#
#Category Totals#
#---------------#

#overall breakdowns + federal and state

haver$health.transfers=haver$Medicaid.Outlays+haver$Medicare.Outlays
haver$federal.health=haver$Medicare.Outlays+haver$federal.medicaid
haver$state.health=haver$health.transfers-haver$federal.health

haver$social.transfers=haver$Total.Gov.t.Benefits.to.Persons-haver$Medicare.Outlays-haver$Medicaid.Outlays
haver$federal.social=haver$federal.social.ex.health.share*haver$social.transfers
haver$state.social=haver$social.transfers-haver$federal.social

haver$non.corp.taxes=haver$Contributions.for.Government.Social.Insurance+haver$Personal.Current.Taxes+haver$Taxes.on.Production.and.Imports
haver$state.non.corp.taxes=haver$State.Personal.Current+haver$State.Taxes.on.production+haver$State.Contributions.for.social.insurance
haver$federal.non.corp.taxes=haver$non.corp.taxes-haver$state.non.corp.taxes

haver$corp.taxes.adj=haver$Corporate.Taxes#-haver$Corporate.Taxes.from.Fed.Banks
haver$federal.corp.taxes=haver$corp.taxes.adj-haver$State.Corporate.taxes

#######################
#automatic stabilizers#
#######################

#For more on automatic stabilizers and the FIM, see Sheiner and Ng (2019).

#stabilizers from corporate taxes are the share that does not grow with potential, assuming a flat rate for both federal and state
haver$federal.stabilizers.corp.taxes=haver$Federal.Corporate.Taxes-haver$federal.corp.tax.rate*haver$CBO.Nominal.Potential.GDP
haver$state.stabilizers.corp.taxes=haver$State.Corporate.taxes-haver$state.corp.tax.rate*haver$CBO.Nominal.Potential.GDP

#stabilizers from non-corporate taxes for federal are taken from the CBO whereas State non-corporate taxe rates are assumed to be fixed (see Sheiner [2019]: https://piie.com/publications/working-papers/effects-low-productivity-growth-fiscal-sustainability-united-states)
haver$federal.stabilizers.non.corp.taxes=haver$cbo.stabilizers.revenue-haver$federal.stabilizers.corp.taxes
haver$state.stabilizers.non.corp.taxes=haver$state.non.corp.taxes-haver$state.ind.tax.rate*haver$CBO.Nominal.Potential.GDP

haver$federal.stabilizers.revenue=haver$federal.stabilizers.corp.taxes+haver$federal.stabilizers.non.corp.taxes
haver$state.stabilizers.revenue=haver$state.stabilizers.corp.taxes+haver$state.stabilizers.non.corp.taxes

#----------------------------------#
#Stabilizer Regression for Medicaid#
#----------------------------------#

#CBO computes automatic stabilizers using a regresison based on the portion of spending growing with the unemployment gap. We run our own regression for total Medicaid instead of only Federal Medicaid (which the CBO does) and then take federal and state components. See CBO (2015) here: https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/workingpaper/51005-AutomaticStabilizers.pdf

#the automatic stabilizer is the total minus the share that you would expect if you were at full employment (i.e. what the model would predict when the unemployment gap is 0; the model residual)

medicaid.reg.ugap=lm(diff((Medicaid.Outlays/potential_gdp)*100)~diff(ugap)+shift(diff(ugap),1)+shift(diff(ugap),2),data=haver[haver$date>="1970-01-01" & haver$date<"2018-10-01",])
medicaid.stab=numeric()
for (k in 5:nrow(haver)){
  medicaid.stab[1:4]=((haver[1:4,"Medicaid.Outlays"]/haver$potential_gdp[1:4])*100)
  medicaid.stab[k]=medicaid.stab[k-1]+c(coef(medicaid.reg.ugap)[1]+resid(medicaid.reg.ugap))[k-4]
}
haver$medicaid.as=haver$Medicaid.Outlays-(medicaid.stab*haver$potential_gdp/100)
haver$state.medicaid.as=haver$medicaid.as*(1-haver$federal.medicaid.share.adj.ca)
haver$federal.medicaid.as=haver$medicaid.as*(haver$federal.medicaid.share.adj.ca)

#state stabilizers in transfers are only Medicaid, where the FMAPs are adjusted to exclude the temporary increases in state Medicaid allocations in 2003-2004 and 2007-2011. We do not include other state transfers such as SNAP since they are a small share. Other benefits such as medicare and social security aren't cyclical and so aren't computed.
haver$state.stabilizers.transfers=haver$state.medicaid.as

#federal stabilizers in transfers are Medicaid and UI, where UI is adjsuted to exclude Emergency Unemployment Compensation. we attrribute all of UI to federal due to data limitations, even though in reality it is split. We do not include EITC rebates or SNAP.
haver$Unemployment.Insurance.less.euc=haver$Unemployment.Insurance-haver$euc
ui.reg.ugap=lm(diff((Unemployment.Insurance.less.euc/potential_gdp)*100)~diff(ugap)+shift(diff(ugap),1)+shift(diff(ugap),2),data=haver[haver$date>="1970-01-01" & haver$date<"2018-10-01",])
ui.pred=numeric()
ui.stab=numeric()
for (k in 5:nrow(haver)){
  ui.stab[1:4]=((haver[1:4,"Unemployment.Insurance.less.euc"]/haver$potential_gdp[1:4])*100)
  ui.stab[k]=ui.stab[k-1]+c(coef(ui.reg.ugap)[1]+resid(ui.reg.ugap))[k-4]
}
haver$ui.as=haver$Unemployment.Insurance.less.euc-(ui.stab*haver$potential_gdp/100)
haver$federal.stabilizers.transfers=haver$federal.medicaid.as+haver$ui.as


haver$federal.stabilizers=haver$federal.stabilizers.revenue+haver$federal.stabilizers.transfers
haver$state.stabilizers=haver$state.stabilizers.revenue+haver$state.stabilizers.transfers

#----------#
#Apply MPCs#
#----------#

#spending MPCs of 0.9 applied over the previous 4-quarter average
spending.mpc=function(df=haver,x="health.transfers"){
  rollapply(df[,x],width=4,FUN=function(y) mean(y,na.rm=T),fill=NA,by.column=FALSE,align="right")*0.9
}

#Induvidual Income MPC of 0.6; split between equal weighting in most recent 2 quarters and remaining over previous 6 quarters
non.corp.tax.mpc=function(df=haver,x="non.corp.taxes"){
  six.qtr=rollapply(df[,x],width=6,FUN=function(y) mean(y,na.rm=T),fill=NA,by.column=FALSE,align="right")*0.36
  output=numeric(nrow(df))
  for (i in 3:nrow(df)){
    output[i]=six.qtr[i-2]+df[i-1,x]*0.12+df[i,x]*0.12
  }
  output=output*(-1)
  return(output)
}

#corporate tax MPC of 0.4 over the previous 12-quarter average
corp.tax.mpc=function(df=haver,x="corp.taxes.adj"){
  rollapply(df[,x],width=12,FUN=function(y) mean(y,na.rm=T),fill=NA,by.column=FALSE,align="right")*(-0.4)
}

spending=c("health.transfers","federal.health","state.health","social.transfers","federal.social","state.social","federal.stabilizers.transfers","state.stabilizers.transfers","state.stabilizers.transfers")
non.corp.tax=c("non.corp.taxes","state.non.corp.taxes","federal.non.corp.taxes","federal.stabilizers.non.corp.taxes","state.stabilizers.non.corp.taxes")
corp.tax=c("corp.taxes.adj","federal.corp.taxes","State.Corporate.taxes","federal.stabilizers.corp.taxes","state.stabilizers.corp.taxes")

fim.mpc=data.frame(date=haver$date)
for (i in 1:length(spending)){
  fim.mpc[,spending[i]]=spending.mpc(x=spending[i])
}

for (i in 1:length(non.corp.tax)){
  fim.mpc[,non.corp.tax[i]]=non.corp.tax.mpc(x=non.corp.tax[i])
}

for (i in 1:length(corp.tax)){
  fim.mpc[,corp.tax[i]]=corp.tax.mpc(x=corp.tax[i])
}

#sum up induvidual categories into sub and overarching classifications
fim.mpc$federal.c=rowSums(fim.mpc[,c("federal.health","federal.social","federal.non.corp.taxes","federal.corp.taxes")])
fim.mpc$state.c=rowSums(fim.mpc[,c("state.health","state.social","state.non.corp.taxes","State.Corporate.taxes")])

fim.mpc$federal.taxes=rowSums(fim.mpc[,c("federal.corp.taxes","federal.non.corp.taxes")])
fim.mpc$state.taxes=rowSums(fim.mpc[,c("State.Corporate.taxes","state.non.corp.taxes")])

fim.mpc$federal.transfers=rowSums(fim.mpc[,c("federal.health","federal.social")],na.rm=T)
fim.mpc$state.transfers=rowSums(fim.mpc[,c("state.health","state.social")],na.rm=T)

fim.mpc$federal.stabilizers=rowSums(fim.mpc[,c("federal.stabilizers.transfers","federal.stabilizers.corp.taxes","federal.stabilizers.non.corp.taxes")])
fim.mpc$state.stabilizers=rowSums(fim.mpc[,c("state.stabilizers.transfers","state.stabilizers.corp.taxes","state.stabilizers.non.corp.taxes")])
fim.mpc$stabilizers=rowSums(fim.mpc[,c("federal.stabilizers","state.stabilizers")])

fim.mpc$federal.stabilizers.taxes=rowSums(fim.mpc[,c("federal.stabilizers.corp.taxes","federal.stabilizers.non.corp.taxes")])
fim.mpc$state.stabilizers.taxes=rowSums(fim.mpc[,c("state.stabilizers.corp.taxes","state.stabilizers.non.corp.taxes")])
fim.mpc$stabilizers.taxes=rowSums(fim.mpc[,c("federal.stabilizers.taxes","state.stabilizers.taxes")])

fim.mpc$transfers=fim.mpc$health.transfers+fim.mpc$social.transfers
fim.mpc$taxes=fim.mpc$corp.taxes.adj+fim.mpc$non.corp.taxes
fim.mpc$consumption=rowSums(fim.mpc[,c("health.transfers","social.transfers","corp.taxes.adj","non.corp.taxes")])

#---------------------------------------------#
#Cotribution of Consumption Growth to Real GDP#
#---------------------------------------------#

#contribution to consumption is the real change in each component as a share of previous period nominal GDP, annualized. Importantly, the FIM is indexed to 'neutral' based on growth in real potential GDP computed by the CBO. That is, a positive FIM of 1 percentage points means it's adding to GDP by 1 percentage points above potential (or 1pp towards the output gap).

#Importantly, the old FIM used the PCE deflator from table 1.1.9 from the NIPAs. The new FIM uses the Chained Price Indexes from table 1.1.3 which is the main index by which the NIPAs are actually adjsuted. Though very similar, the two indices do vary somewhat (see: http://piketty.pse.ens.fr/files/capitalisback/CountryData/USA/Methodo/NIPA%20Guide.pdf or https://www.bea.gov/sites/default/files/methodologies/nipa-handbook-all-chapters.pdf#page=64),

c.contribution=function(df=fim.mpc,x="consumption"){
  output=NA
  target=NA
  for (i in 2:nrow(df)){
    target[i]=(haver$CBO.Nominal.Potential.GDP[i]/haver$CBO.Nominal.Potential.GDP[i-1])-(haver$GDP.deflator[i]/haver$GDP.deflator[i-1])+(haver$PCE.deflator[i]/haver$PCE.deflator[i-1])
    output[i]=400*((df[i,x]-target[i]*df[i-1,x])/haver$Nominal.GDP[i-1])
  }
  return(output)
}

fim.c=data.frame(date=haver$date)
for (i in 2:ncol(fim.mpc)){
  fim.c[,colnames(fim.mpc)[i]]=c.contribution(x=colnames(fim.mpc)[i])
}

#sum up categories
fim.c$consumption.discres=fim.c$consumption-fim.c$stabilizers

fim.c$taxes.discres=fim.c$taxes-fim.c$stabilizers.taxes
fim.c$federal.taxes.discres=fim.c$federal.taxes-fim.c$federal.stabilizers.taxes
fim.c$state.taxes.discres=fim.c$state.taxes-fim.c$state.stabilizers.taxes

fim.c$stabilizers.transfers=fim.c$federal.stabilizers.transfers+fim.c$state.stabilizers.transfers

fim.c$transfers.discres=fim.c$transfers-fim.c$stabilizers.transfers
fim.c$federal.transfers.discres=fim.c$federal.transfers-fim.c$federal.stabilizers.transfers
fim.c$state.transfers.discres=fim.c$state.transfers-fim.c$state.stabilizers.transfers

print("FIM Consumption Done")

#We also include below the method for computing an alternate version of the FIM, which does not use potential growth as a neutral baseline. Instead the baseline is that the government does not exist; It is computed based on the growth rate of real taxes and transfers as a share of the previous period Nominal GDP. This version most closley approximates the 'old' FIM that computes consumption based on changes in real government expenidture relative to changes in real PCE times the nominal share of consumption in GDP. 


c.nogov.contribution=function(df=fim.mpc,x="consumption"){
  output=NA
  for (i in 2:nrow(df)){
    output[i]=400*((df[i,x]-((haver$PCE.deflator[i]/haver$PCE.deflator[i-1])*df[i-1,x]))/haver$Nominal.GDP[i-1])
  }
  return(output)
}


fim.c.nogov=data.frame(date=haver$date)
for (i in 2:ncol(fim.mpc)){
  fim.c.nogov[,colnames(fim.mpc)[i]]=c.nogov.contribution(x=colnames(fim.mpc)[i])
}

#sum up categories
fim.c.nogov$consumption.discres=fim.c.nogov$consumption-fim.c.nogov$stabilizers

fim.c.nogov$taxes.discres=fim.c.nogov$taxes-fim.c.nogov$stabilizers.taxes
fim.c.nogov$federal.taxes.discres=fim.c.nogov$federal.taxes-fim.c.nogov$federal.stabilizers.taxes
fim.c.nogov$state.taxes.discres=fim.c.nogov$state.taxes-fim.c.nogov$state.stabilizers.taxes

fim.c.nogov$stabilizers.transfers=fim.c.nogov$federal.stabilizers.transfers+fim.c.nogov$state.stabilizers.transfers

fim.c.nogov$transfers.discres=fim.c.nogov$transfers-fim.c.nogov$stabilizers.transfers
fim.c.nogov$federal.transfers.discres=fim.c.nogov$federal.transfers-fim.c.nogov$federal.stabilizers.transfers
fim.c.nogov$state.transfers.discres=fim.c.nogov$state.transfers-fim.c.nogov$state.stabilizers.transfers

#-------------------------------------#
#Cotribution of Government to Real GDP#
#-------------------------------------#

g.contribution=function(df=haver,x="X.G...Government.Consumption.and.Investment",deflator1="GDP.deflator",deflator2="G.deflator"){
  output=NA
  target=NA
  for (i in 2:nrow(df)){
    target[i]=(haver$CBO.Nominal.Potential.GDP[i]/haver$CBO.Nominal.Potential.GDP[i-1])-(haver[i,deflator1]/haver[i-1,deflator1])+(haver[i,deflator2]/haver[i-1,deflator2])
    output[i]=400*((df[i,x]-target[i]*df[i-1,x])/haver$Nominal.GDP[i-1])
  }
  return(output)
}

fim.c$G.purchases=g.contribution(x="X.G...Government.Consumption.and.Investment")
fim.c$federal.purchases=g.contribution(x="new.federal",deflator2="Federal.deflator")
fim.c$federal.purchases.exdef=g.contribution(x="new.federal.exdef",deflator2="Federal.deflator")
fim.c$state.purchases=g.contribution(x="new.state",deflator2="State.deflator")
fim.c$G.purchases.exdef=fim.c$state.purchases+fim.c$federal.purchases.exdef
fim.c$G.purchases.def=g.contribution(x="Nominal.G.Federal.Defense")
fim.c$Contribution.to..Ch.in.Real.GDP.from..G.=haver$Contribution.to..Ch.in.Real.GDP.from..G.

fim.c$total=fim.c$federal.purchases+fim.c$state.purchases+fim.c$consumption
fim.c$federal=fim.c$federal.purchases+fim.c$federal.c
fim.c$state=fim.c$state.purchases+fim.c$state.c

fim.c$federal.discres=fim.c$federal.c-fim.c$federal.stabilizers+fim.c$federal.purchases
fim.c$state.discres=fim.c$state.c-fim.c$state.stabilizers+fim.c$state.purchases
fim.c$discres=fim.c$state.discres+fim.c$federal.discres

print("FIM Purchases Done")

write.csv(fim.c,paste0(path,'/fim.potential.csv'),row.names = FALSE,na="")

#Compute Government contributipon the 'old way' i.e. mimicking the BEA's contribution to Real GDP from government

g.contribution.nogov=function(x="X.G...Government.Consumption.and.Investment",df=haver,deflator="G.deflator"){
  output=NA
  for (i in 2:nrow(df)){
    output[i]=400*((df[i,x]-df[i-1,x]*(haver[i,deflator]/haver[i-1,deflator]))/haver$Nominal.GDP[i-1])
  }
  return(output)
}

fim.c.nogov$G.purchases=g.contribution.nogov(x="X.G...Government.Consumption.and.Investment")
fim.c.nogov$federal.purchases=g.contribution.nogov(x="new.federal",deflator="Federal.deflator")
fim.c.nogov$federal.purchases.exdef=g.contribution.nogov(x="new.federal.exdef",deflator="Federal.deflator")
fim.c.nogov$state.purchases=g.contribution.nogov(x="new.state",deflator="State.deflator")
fim.c.nogov$G.purchases.exdef=fim.c.nogov$state.purchases+fim.c.nogov$federal.purchases.exdef
fim.c.nogov$G.purchases.def=g.contribution.nogov(x="Nominal.G.Federal.Defense")

fim.c.nogov$federal.purchases.bea=g.contribution.nogov(x="Nominal.G.Federal",deflator="Federal.deflator")
fim.c.nogov$state.purchases.bea=g.contribution.nogov(x="Nominal.G.State",deflator="State.deflator")

fim.c.nogov$total=fim.c.nogov$federal.purchases+fim.c.nogov$state.purchases+fim.c.nogov$consumption
fim.c.nogov$federal=fim.c.nogov$federal.purchases+fim.c.nogov$federal.c
fim.c.nogov$state=fim.c.nogov$state.purchases+fim.c.nogov$state.c

fim.c.nogov$federal.discres=fim.c.nogov$federal.c-fim.c.nogov$federal.stabilizers+fim.c.nogov$federal.purchases
fim.c.nogov$state.discres=fim.c.nogov$state.c-fim.c.nogov$state.stabilizers+fim.c.nogov$state.purchases
fim.c.nogov$discres=fim.c.nogov$state.discres+fim.c.nogov$federal.discres

fim.c.nogov$total.nogov=fim.c.nogov$federal.purchases.bea+fim.c.nogov$state.purchases.bea+fim.c.nogov$consumption
fim.c.nogov$federal.nogov=fim.c.nogov$federal.purchases.bea+fim.c.nogov$federal.c
fim.c.nogov$state.nogov=fim.c.nogov$state.purchases.bea+fim.c.nogov$state.c

fim.c.nogov$federal.nogov.discres=fim.c.nogov$federal.c-fim.c.nogov$federal.stabilizers+fim.c.nogov$federal.purchases.bea
fim.c.nogov$state.nogov.discres=fim.c.nogov$state.c-fim.c.nogov$state.stabilizers+fim.c.nogov$state.purchases.bea
fim.c.nogov$discres.nogov=fim.c.nogov$state.discres+fim.c.nogov$federal.discres

write.csv(fim.c.nogov,paste0(path,'/fim.original.csv'),row.names = FALSE,na="")

#############################
#Regressions and Cyclicality#
#############################

#below we run some regressions of the FIM on the Unemployment Gap computed from CBO, i.e. getting a sense of how countercyclical fiscal policy is as well as what we would've predicted Fiscal policy to be if it were consistent with it's hsitorical response to recessions. We have a separate regression estimated prior to the 2011-2018 period due to the atypical pro-cyclial response as a result of the Budget Control Act (see Sheiner and Ng [2019])

fim.c$date=as.Date(as.yearqtr(fim.c$date))
fim.c1=merge(fim.c,haver[,c("date","ugap","nominal_output_gap","unemp","potential_gdp","tenyr","sp500","ff","X3mo")],by="date",all=TRUE)
fim.c1$ugap.4qtr=fim.c1$ugap-shift(fim.c1$ugap,n=4)
fim.c1$nominal_output_gap.4qtr=fim.c1$nominal_output_gap-shift(fim.c1$nominal_output_gap,n=4)
fim.c1$ugap.1qtr=fim.c1$ugap-shift(fim.c1$ugap,n=1)
fim.c1$nominal_output_gap.1qtr=fim.c1$nominal_output_gap-shift(fim.c1$nominal_output_gap,n=1)

fim.c1$ugap.l1=shift(fim.c1$ugap,n=1)
fim.c1$ugap.l2=shift(fim.c1$ugap,n=2)

fim.c1$ugap.1qtr.l1=shift(fim.c1$ugap.1qtr,n=1)
fim.c1$ugap.4qtr.l1=shift(fim.c1$ugap.4qtr,n=1)
fim.c1$ugap.4qtr.l4=shift(fim.c1$ugap.4qtr,n=4)

fim.c1$ygap.l1=shift(fim.c1$nominal_output_gap,n=1)
fim.c1$ygap.l2=shift(fim.c1$nominal_output_gap,n=2)

fim.c1$unemp.d4=fim.c1$unemp-shift(fim.c1$unemp,n=4)

fim.c1$potential_gdp.d.ann=100*(((fim.c1$potential_gdp/shift(fim.c1$potential_gdp,n=1))^4)-1)
fim.c1$potential_gdp.d.yoy=as.numeric(Delt(fim.c1$potential_gdp,k=4))*100

total.reg=lm(total~ugap.4qtr,data=fim.c1[fim.c1$date>="1980-01-01",])
total.reg.2010=lm(total~ugap.4qtr,data=fim.c1[fim.c1$date>="1980-01-01" & fim.c1$date<="2010-10-01",])
fim.c1$total.ma=rollapply(fim.c1$total,width=4,FUN=function(y) mean(y,na.rm=T),fill=NA,by.column=FALSE,align="right")
fim.c1$total.pred=NA
fim.c1$total.pred[fim.c1$date>="1980-01-01" & fim.c1$date<="2019-01-01"]=predict(total.reg)
fim.c1$total.pred.2010=NA
fim.c1$total.pred.2010[fim.c1$date>="1980-01-01" & fim.c1$date<="2010-10-01"]=predict(total.reg.2010)
fim.c1$total.pred.2010[fim.c1$date>"2010-01-01"]=coef(total.reg.2010)[1]+coef(total.reg.2010)[2]*fim.c1$ugap.4qtr[fim.c1$date>"2010-01-01"]

fim.c1$total.resid=NA
fim.c1$total.resid[fim.c1$date>="1980-01-01" & fim.c1$date<="2019-01-01"]=fim.c1$total[fim.c1$date>="1980-01-01" & fim.c1$date<="2019-01-01"]-fim.c1$total.pred[fim.c1$date>="1980-01-01" & fim.c1$date<="2019-01-01"]

write.csv(fim.c1,paste0(path,'/fim.potential.macro.csv'),row.names = FALSE,na="")
print("FIM Government Done")

