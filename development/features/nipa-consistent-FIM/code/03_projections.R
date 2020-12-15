## 4.2 FIM component calculations ----------------------------------------------------------------------------
# 4.2.3 Growth Rates Assumptions -----------------------------------------------------------------------------------------
## Louise override CBO growth rate for S&L purchases for Q3 and Q4 for 2020
Q3_2020 <- "2020-09-30" 
Q4_2020 <- "2020-12-31" 
## Louise override CBO growth rate for S&L purchases for Q42020 through (& including) Q1 2022
xx$gs_g[204:209] = c(0.0025,0.0025,0.0025,0.005,0.0075,0.01)

# After expiration date, CBO grows fed purchases with inflation. we want to
# We grow federal purchases with nominal potential. 
capExpiration <- "2021-09-30"

# Make special assumptions for projected growth rates
xx <- xx %>% mutate(
  # PURCHASES
  # Federal
  gf_g = if_else(date > capExpiration,
                 gdppothq_g + jgdp_g,
                 gf_g),
  # State & Local
  # Note: Louise said to override CBO growth rate for S&L purchases for Q3 and Q4 for 2020
  
  # TRANSFERS
  
  # Federal
  # Net federal transfers grow at the same rate as gross federal transfers
  gftfpnet_g = gftfp_g, 
  
  # State & Local
  # S&L gross and net transfers both grow with S&L current expenditures
  gstfp_g = gs_g,
  gstfpnet_g =  gs_g, 
  
  # SUBSIDIES
  # Federal and S&L subsidies grow with potential GDP
  gfsub_g = gdppothq_g,
  gssub_g = gdppothq_g,
  
  # GRANTS
  # Federal
  #  Health & Hospital grants to states growth with Medicaid
  gfeghhx_g = yptmd_g, 
  # Medicaid grants to states grow with medicaid
  gfeghdx_g = yptmd_g, 
  
  # Aid to S&L grow with federal purchases
  gfeg_g = gf_g, 
  # Capital grants to state and local gov'ts grow with federal purchases
  gfeigx_g = gf_g, 
  
  # DEFLATORS
  # State & Local
  # Investment deflator grows with overall deflator for S&L
  jgsi_g = jgs_g,
  # Consumption deflator grows with overall deflator for S&L
  jgse_g = jgs_g
  
  # Disaggregated medicaid components grow with the aggregate  
  # yfptmd_g = yptmd_g,  
  # ysptmd_g = yptmd_g, 
)


# 5 Forecast ----------------------------------------------------------------------------------------------------

## Generate forecastPeriod values of components using current levels and our projected growth rates. 

components <-
  c(
    # GDP
    ## Actual
    'gdp', 'gdph', 'jgdp',
    ## Potential
    'gdppotq', 'gdppothq',
    # Gov't Consumption Expenditures & Gross Investment 
    ## Total 
    'g', 'gf', 'gs', 
    ## Deflators
    ### Total
    'jgf', 'jgs',
    ### S&L Consumption/Investment Expenditures
    'jgse', 'jgsi',
    # GRANTS
    ## Total
    'gfeg', 
    ### Health & Hospitals
    'gfeghhx',
    ### Medicaid
    'gfeghdx', 
    ### Investment
    'gfeigx', 
    # TAXES
    ## Personal
    'gfrpt', 'gsrpt',
    ## Social Insurance
    'gfrs' ,'gsrs', 
    ## Corporate
    'gfrcp', 'gsrcp',
    ## Production & Imports
    'gfrpri', 'gsrpri',
    # SOCIAL BENEFITS
    ## Total
    'gftfp', 'gstfp',
    ## Medicaid
    'yptmd',
    ## Medicare
    'yptmr',
    # SUBSIDIES 
    'gssub', 'gfsub', 
    # PERSONAL CONSUMPTION
    'c', 'jc'
  )
# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
forecastPeriod <- which(xx$date > last_hist_date)

for(f in forecastPeriod){
  xx[f,components] = xx[f-1, components]  * (1 + xx[f, paste0(components, "_g")])
}
xx$gtfp[forecastPeriod] = xx$gftfp[forecastPeriod] + xx$gstfp[forecastPeriod] # social benefits = federal benefits + state and local benefits
xx$yptx[forecastPeriod] = xx$gfrpt[forecastPeriod] + xx$gsrpt[forecastPeriod] # alternative path
xx$yptxb[forecastPeriod] = xx$gfrptCurrentLaw[forecastPeriod] + xx$gsrpt[forecastPeriod] # current law
xx$ytpi[forecastPeriod] = xx$gsrpri[forecastPeriod] + xx$gfrpri[forecastPeriod]  #production and import taxes
xx$grcsi[forecastPeriod] = xx$gsrs[forecastPeriod] + xx$gfrs[forecastPeriod]  # payroll taxes
xx$yctlg[forecastPeriod] = xx$gsrcp[forecastPeriod] + xx$gfrcp[forecastPeriod] # corporate taxes
xx$gsub[forecastPeriod] = xx$gssub[forecastPeriod] + xx$gfsub[forecastPeriod] # subsidies

# Medicaid grants reallocation ----------------------------------------------------------------
# Only relevant because we want to take federal medicaid grants 
# out of federal consumption grants

 xx <-
   xx %>%
    mutate(
#       
#       
#       # Reattribute federal grants to states back to Federal government
#       # Parse between those for consumption and investment and those for transfers (Medicaid)
#       
#       # federal medicaid grants to states
      fshare = fmap$fshare[match(year(date), fmap$year)] %>%
        na.locf(),
      yfptmd = if_else(is.na(gfeghdx), # if we don't have the medicaid data (pre-1993)'
                       yptmd*fshare, # use the fmaps to estimate
                       gfeghdx)# otherwise, use data for medicaid + prescription drugs transfers
#       # 
#       # 
#       # state medicaid payments = total medicaid - federal medicaid grants
#       # ysptmd = yptmd - yfptmd,
#       # gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
#       
#       # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
#       # gstfpnet = gstfp - yfptmd, 
#       # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
#       # gftfpnet = gftfp + yfptmd 
#       # we reattribute the capital grants later after calculating contributions. 
)

