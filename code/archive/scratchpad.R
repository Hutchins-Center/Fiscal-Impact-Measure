

fim = fim %>% mutate(
  gdppotha = (((gdppoth+1)^4)-1)*100,
  fedneutral = gdppotha*lag(federal_nom,1)/lag(gdp,1),
  slneutral = gdppotha*lag(state_local_nom,1)/lag(gdp,1),
  
  fedigrantsneutral = gdppotha*lag(federal_igrants,1)/lag(gdp,1),
  fedcgrantsneutral = gdppotha*lag(federal_cgrants,1)/lag(gdp,1),
  
  fedneutral_a = fedneutral + fedigrantsneutral + fedcgrantsneutral,
  slneutral_a = slneutral - (fedigrantsneutral + fedcgrantsneutral),
  
  fedcontbea = federal_cont + fedneutral_a,# federal_cont_0 + fedneutral,
  slcontbea = state_local_cont + slneutral_a, #state_local_cont_0 + slneutral,
  gcontbea = slcontbea + fedcontbea
)


tail(fim %>% select(c(date, contains("bea"))),10)

# With Addons
remove <- c('state_local_cont','federal_cont','taxes_transfers_cont','subsidies_cont','recession', 'pi_federal', 'pi_state_local_c', 'pi_state_local_i', 'pi_gdp','pi_pce','pi_state_local', 'fim_bars','fim_bars_ma')
keep <- c('date','federal_noncorp_taxes','state_noncorp_taxes', 'federal_corporate_taxes','state_corporate_taxes','federal_health_outlays','federal_social_benefits','state_health_outlays','state_social_benefits','subsidies','federal_cgrants', 'federal_nom','state_local_nom')


fim_w_addon <- fim %>%   filter(date > "2019-12-31" & date <= "2021-12-31") %>% select(keep)
fim_w_addon

write.xlsx(fim_w_addon, "Covid Changes July 2020/fim_with_addons.xlsx")

# No Addons

remove <- c('state_local_cont','federal_cont','taxes_transfers_cont','subsidies_cont','recession', 'pi_federal', 'pi_state_local_c', 'pi_state_local_i', 'pi_gdp','pi_pce','pi_state_local', 'fim_bars','fim_bars_ma')

keep <- c('date','federal_noncorp_taxes','state_noncorp_taxes', 'federal_corporate_taxes','state_corporate_taxes','federal_health_outlays','federal_social_benefits','state_health_outlays','state_social_benefits','subsidies','federal_cgrants', 'federal_nom','state_local_nom')
fim_no_addon <- fim %>%   filter(date > "2019-12-31" & date <= "2021-12-31") %>% select(keep)
fim_no_addon

write.xlsx(fim_no_addon, "Covid Changes July 2020/no_addons.xlsx")


#Social Benefits

#Federal

#Variables used to calculate yfptmd
View(xx[,c('date','yfptmd','yptmd','fshare','gfeghdx', 'gfeghhx')])

#gftfp is calculated by taking out medicare, medicaid, and UI (and smoothing them), taking off the COLA portion,
#smoothing,  adding cola back in, adding smooth medicare, medicaid, and UI back in.

#reattribute state unemployment from federal back to state
#includes federal UI which we need to take out 768.8 from Q2

#Note: gftfbusx is 1048.242 for Q2
xx$gftfp = xx$gftfp - xx$gftfbusx # + 768.8 (for Q2)

gftfpnet = gftfp + yfptmd # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government

federal_social_benefits = gftfpnet - yfptmd - ytptmr

#State
xx$gstfp = xx$gstfp + xx$gftfbusx # - 768.8
xx$gstfp[202] = xx$gstfp[202] -768.8

gstfpnet = gstfp - yfptmd
#net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government


state_social_benefits = xx$gstfpnet - xx$ysptmd


#Health

#Federal
federal_health_outlays = xx$yfptmd + xx$yptmr

#State
state_health_outlays = xx$ysptmd # only includes medicaid 
ysptmd = yptmd - yfptmd




#Why is Q4 of 2022 so high?

View(fim[,c('date','state_local_cont','federal_cont','taxes_transfers_cont','subsidies_cont','fim_bars')] %>% filter(date > "2020-03-31"))

#federal_cont and taxes_transfers_cont are very high
#state_local_cont very low

View(fim[,c('date','federal_cont','federal_nom','federal_cgrants','federal_cgrants_cont','federal_igrants','federal_igrants_cont')] %>% filter(date > "2020-03-31"))





library('DataExplorer')
fim_08.2020 <- read_csv("08-2020/fim-projections-2020-08-04.csv") 

fim_08.2020 <- fim_08.2020 %>% select(-X1)


fim_diff <- fim - fim_08.2020
plot_histogram(fim_diff)



