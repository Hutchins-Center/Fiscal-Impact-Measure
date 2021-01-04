#source('code/00_pre-process.R')
source('code/01_datapull.R')
source('code/02_projections.R')
source('code/03_calculations.R')
source('code/04_figures.R')
source('code/05_renderFigures.R')

# No stimulus
source('code/01_datapull.R')
source('code/02_projections_nostimulus.R')
source('code/03_calculations_nostimulus.R')
source('code/04_figures.R')
source('code/05_renderFigures_nostimulus.R')

# NO ADDONS
source('code/01_datapull.R')
source('code/02_projections.R')
source('code/03_calculations_noaddons.R')
source('code/04_figures.R')

# 
# fim_noaddons <- read_excel("results/12-2020/no-addons/fim_noaddons.xlsx")
# fim_noaddons %>%
#   select(date, state_health_outlays,
#          state_social_benefits,
#          state_noncorp_taxes,
#          state_corporate_taxes,
#          state_subsidies,
#          federal_health_outlays,
#          federal_social_benefits,
#          federal_noncorp_taxes,
#          federal_corporate_taxes,
#          federal_subsidies,
#          federal_cgrants) %>%
#   mutate(date = as.Date(date)) %>%
#   filter(date >= '2020-03-30') %>%
#   write_xlsx('results/12-2020/no-addons/fim_noaddons_summary.xlsx')
