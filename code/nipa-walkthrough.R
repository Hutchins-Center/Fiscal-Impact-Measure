# Setup ---------------------------------------------------------------------------------------
library('lubridate')
library('tsibble')
library('snakecase')
library("r2excel")
library('drake')
source('R/packages.R')
loadd(nipa_projections)
loadd(fim)
to_table <- function(df) {
  df %>%
    rename_with(.fn = ~ to_title_case(.),
                .cols = where(is.numeric)) %>%
    pivot_longer(where(is.numeric), names_to = ' ') %>%
    pivot_wider(names_from = date, values_from = value)
}
make_excel_table <- function(df, title,  rowcolors){
  xlsx.addHeader(
    wb,
    sheet,
    value = title,
    level = 1,
    color = "black",
    underline = 1
  )
  
  xlsx.addTable(
    wb,
    sheet,
    data = df,
    fontSize = 12,
    rowFill = rowcolors
  )
  
  xlsx.addLineBreak(sheet, 2)
}

add_factors <- readxl::read_xlsx('data/add-ons/LSFIM_KY_v7.xlsx', 
                         sheet = 'FIM Add Factors')
# Clean data ----------------------------------------------------------------------------------
nipa <-
  nipa_projections %>%
  filter(date >= '2018-03-31' & date <= '2022-12-31') %>%
  left_join(add_factors %>%
              select(date, contains(c(
                'purchases', 'grants'
              )))) %>%
  mutate(across(starts_with('add'), ~ coalesce(., 0))) %>%
  transmute(
    date = date,
    gross_domestic_product = gdp,
    
    federal_purchases = gf + add_federal_purchases,
    state_purchases = gs + add_state_purchases,
    consumption_grants_gross = gfeg,
    medicaid_grants = gfeghdx,
    consumption_grants_net = add_federal_cgrants + consumption_grants_gross - medicaid_grants,
    investment_grants = gfeigx,
    
    federal_purchases_deflator_growth = q_g(jgf),
    state_purchases_deflator_growth = q_g(jgs),
    consumption_grants_net_deflator_growth = q_g(jgse),
    investment_grants_deflator_growth = q_g(jgsi),
    real_potential_gdp_growth = q_g(gdppothq),
    
    federal_purchases_growth = gf_g,
    state_purchases_growth = gs_g,
    consumption_grants_gross_growth = gfeg_g,
    medicaid_grants_growth = gfeghhx_g,
    investment_grants_growth = gfeigx_g
  ) %>%
  mutate(date  = yearquarter(date),
         across(where(is.numeric), ~ round(., digits = 2)))
# Get Contributions ---------------------------------------------------------------------------
nipa_consistent_fim <-
  nipa %>%
  mutate(
    over(
      c(
        'federal_purchases',
        'state_purchases',
        'consumption_grants_net',
        'investment_grants'
      ),
      ~ 400 * (.("{.x}") - lag(.("{.x}")) * (1 + .(
        "{.x}_deflator_growth"
      )))  / lag(gross_domestic_product),
      .names = "{x}_contribution")
    ) %>%
      mutate(grants_contribution = consumption_grants_net_contribution + investment_grants_contribution)
  
fim <-
  nipa %>%
  mutate(
    over(
      c(
        'federal_purchases',
        'state_purchases',
        'consumption_grants_net',
        'investment_grants'
      ),
      ~ 400 * (.("{.x}") - lag(.("{.x}")) * (
        1 + .("{.x}_deflator_growth") + real_potential_gdp_growth
      ))  / lag(gross_domestic_product),
      .names = "{x}_contribution"),
    ) %>%
      mutate(
        grants_contribution = consumption_grants_net_contribution + investment_grants_contribution,
        federal_contribution = federal_purchases_contribution + grants_contribution,
        state_contribution = state_purchases_contribution - grants_contribution
      )
# Get tables ----------------------------------------------------------------------------------
nipa_consistent_fim_inputs <-
  nipa_consistent_fim %>%
  select(-ends_with(c('contribution',  'growth'))) %>%
  to_table()

nipa_consistent_fim_growth <-
  nipa_consistent_fim %>%
  select(date, ends_with('growth'), -contains('deflator')) %>%
  to_table()

nipa_consistent_fim_deflators <-
  nipa_consistent_fim %>%
  select(date, ends_with('deflator_growth')) %>%
  to_table()
nipa_consistent_fim_contributions <-
  nipa_consistent_fim %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
  select(
    date,
    federal_purchases_contribution,
    state_purchases_contribution,
    grants_contribution
  ) %>%
  to_table()

fim_contributions <-
  fim %>%
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
  select(date,
         federal_contribution,
         state_contribution,
         grants_contribution) %>%
  to_table()
# Create workbook -----------------------------------------------------------------------------
filename <- "nipa-consistent-fim.xlsx"
wb <- createWorkbook(type = "xlsx")
# Create a sheet in that workbook to contain the data table
sheet <- createSheet(wb, sheetName = "inputs")
contributions_sheet <- createSheet(wb, sheetName = "contributions")

nipa_consistent_fim_inputs %>%
  make_excel_table('Input data', c('white', 'lightblue'))
nipa_consistent_fim_deflators %>%
  make_excel_table('Deflators', c("white", "lightgreen"))
nipa_consistent_fim_growth  %>%
  make_excel_table('Growth rates', c('white', 'turquoise'))
nipa_consistent_fim_contributions %>%
  make_excel_table("Contributions consistent with BEA", c("white", "#CC99FF") )
fim_contributions %>%
  make_excel_table("Contributions using the Fiscal Impact Measure's methodology",
                   c('white', 'orange'))

saveWorkbook(wb, filename)
xlsx.openFile(filename)# view the file
