
# 5 Forecast ----------------------------------------------------------------------------------------------------

## Generate forward values of components using current levels and our projected growth rates. 

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

forecastPeriod <- which(xx$date > last_hist_date)

for(f in forecastPeriod){
  xx[f,components] = xx[f -1, components]  * (1 + xx[f, paste0(components, "_g")])
}
# projections of total tax and transfer pieces = projections of state & local plus federal tax and transfer pieces 
xx <-
  xx %>%
  mutate(
    
    
    gtfp = gftfp + gstfp, # social benefits = federal benefits + state and local benefits
    yptx = gfrpt + gsrpt, # alternative path
    yptxb = gfrptCurrentLaw + gsrpt, # current law
    ytpi = gsrpri + gfrpri,  #production and import taxes
    grcsi = gsrs + gfrs,  # payroll taxes
    yctlg = gsrcp + gfrcp, # corporate taxes
    
    
    # Reattribute federal grants to states back to Federal government
    # Parse between those for consumption and investment and those for transfers (Medicaid)
    
    # federal medicaid grants to states
    yfptmd = if_else(is.na(gfeghdx), # if we don't have the medicaid data (pre-1993)'
                     yptmd*fshare, # use the fmaps to estimate
                     gfeghdx), # otherwise, use data for medicaid + prescription drugs transfers
    
    
    # state medicaid payments = total medicaid - federal medicaid grants
    ysptmd = yptmd - yfptmd,
    gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
    
    # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
    gstfpnet = gstfp - yfptmd, 
    # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
    gftfpnet = gftfp + yfptmd 
    # we reattribute the capital grants later after calculating contributions. 
  )