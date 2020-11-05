
## 4.2 FIM component calculations ----------------------------------------------------------------------------

### 4.2.1 ------------------------------------------------------------------------------------------

# gftfbusx =	Fed Transfer Payments/Persons: State Unemployment Insurance Benefits (SAAR, Mil.$)
# gftfp = Federal Government Social Benefit Payments to Persons (SAAR, Bil.$)
# gstfp = State & Local Government Social Benefit Payments to Persons (SAAR, Bil.$)

# Federal UI legislation total from Q2 of 2020 is $768.8 (Bil. $)
Q2_2020 <- "2020-06-30" 
federal_UI <- 768.8
xx <- 
  xx %>%
  mutate(
    
    # SOCIAL BENEFITS
    # Unemployment Insurance
    gftfbusx = gftfbusx / 1000, # Translate UI from millions to billions
    
    # GRANTS
    
    # Fix units  (millions -> billions)
    # Health & Hospitals grants
    gfeghhx = gfeghhx / 1000,
    # Medicaid grants
    gfeghdx = gfeghdx / 1000,
    # Capital grants to S&L
    gfeigx = gfeigx / 1000,
    
    ## Medicaid
    
    # assume FMAP remains constant -- we still need the fmaps to do pre-1993 reallocation of grants
    fshare = fmap$fshare[match(year(date), fmap$year)] %>%
      na.locf()
  )


# 4.2.3 Growth Rates Assumptions -----------------------------------------------------------------------------------------


## Louise override CBO growth rate for S&L purchases for Q3 and Q4 for 2020

Q3_2020 <- "2020-09-30" 
Q4_2020 <- "2020-12-31" 


# past cap expiration dates, CBO assumes that fed purchases just grow with inflation. 
# we want to assume they grow with nominal potential (zero impact, essentially)
capExpiration <- "2020-09-30"

# Additional component calculations
# Make special assumptions for projected growth rates
xx <- xx %>% mutate(
  # PURCHASES
  # Federal

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


