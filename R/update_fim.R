fim_update <-
  function (df)
  {
    df %>% transmute(
      date = date,
      historical = historical,
      gdp = gdp,
      gdppot = q_g(gdppothq) + q_g(jgdp),
      gdppoth = q_g(gdppothq),
      pi_gdp = q_g(jgdp),
      pce = c,
      pi_pce = q_g(jc),
      recession = if_else(recessq ==
                            -1, 0, recessq),
      federal_nom = gf,
      federal_cgrants_gross = gfeg,
      federal_health_grants = gfeghhx,
      federal_medicaid_grants = yfptmd,
      federal_cgrants = federal_cgrants_gross - federal_medicaid_grants,
      federal_igrants = gfeigx,
      pi_federal = q_g(jgf),
      state_local_nom = gs,
      pi_state_local = q_g(jgs),
      pi_state_local_c = q_g(jgse),
      pi_state_local_i = q_g(jgsi),
      federal_nom_pi = pi_federal,
      state_local_nom_pi = pi_state_local,
      federal_cgrants_pi = pi_state_local_c,
      federal_igrants_pi = pi_state_local_i,
      medicare = yptmr,
      medicaid = yptmd,
      health_outlays = medicare + medicaid,
      gtfp,
      social_benefits_gross = gtfp,
      social_benefits = social_benefits_gross - health_outlays,
      personal_taxes = yptx,
      payroll_taxes = grcsi,
      production_taxes = ytpi,
      noncorp_taxes = personal_taxes + production_taxes + payroll_taxes,
      corporate_taxes = yctlg,
      subsidies = gsub,
      rebate_checks = if_else(is.na(gftfpe),
                              0, gftfpe),
      federal_medicaid = yfptmd,
      federal_health_outlays = medicare +
        federal_medicaid,
      
      federal_rebate_checks = rebate_checks,
      federal_social_benefits = gftfpnet -
        federal_health_outlays,
      federal_personal_taxes = gfrpt,
      federal_payroll_taxes = gfrs,
      federal_production_taxes = gfrpri,
      federal_noncorp_taxes = federal_personal_taxes + federal_payroll_taxes +
        federal_production_taxes,
      federal_corporate_taxes = gfrcp,
      federal_subsidies = gfsub,
      state_medicaid = medicaid -
        federal_medicaid,
      state_health_outlays = state_medicaid,
      state_social_benefits_gross = gstfp,
      state_rebate_checks = 0,
      state_social_benefits = state_social_benefits_gross -
        state_health_outlays - federal_medicaid_grants,
      state_personal_taxes = gsrpt,
      state_payroll_taxes = gsrs,
      state_production_taxes = gsrpri,
      state_noncorp_taxes = state_personal_taxes + state_payroll_taxes +
        state_production_taxes,
      state_corporate_taxes = gsrcp,
      unemployment_insurance,
      federal_unemployment_insurance,
      state_unemployment_insurance,
      state_subsidies = gssub
    )
  }
