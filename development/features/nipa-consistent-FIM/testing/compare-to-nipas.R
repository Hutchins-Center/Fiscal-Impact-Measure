source('src/packages.R')
library('arsenal')
thismonth <- format(Sys.Date(), "%m-%Y")
nipa_path <- "development/features/nipa-consistent-FIM"
nipa_results_path <- paste0(nipa_path, '/results/', thismonth, '/data')


# State ---------------------------------------------------------------------------------------

bea_state <- read_xlsx(here(nipa_path, 'bea-tables', 'state-receipts-expenditures.xlsx')) %>%
  mutate(date = as.Date(date))

bea_start <- min(bea_state$date)
bea_end <- max(bea_state$date)
fim_nipa <- 
  read_xlsx(here(nipa_results_path, 'fim.xlsx')) %>%
  filter(date >= bea_start & date <= bea_end) %>%
  mutate(date = as.Date(date))

fim_nipa_state <-
  fim_nipa %>% 
  select(date, contains('state'), federal_cgrants,
         -contains(c('cont','net', 'add', 'pi'))
         )

summary(
  comparedf(
    fim_nipa_state %>%
    mutate(state_social_benefits = state_social_benefits + state_health_outlays),
                  bea_state,
                  tol.num.val = 0.1)
  )


# Federal -------------------------------------------------------------------------------------

fim_nipa_federal <-
  fim_nipa %>% 
  select(date, contains('federal'),
         -contains(c('cont','net', 'add', 'pi'))
  )
bea_federal <- read_xlsx(here(nipa_path, 'bea-tables', 'federal-receipts-expenditures.xlsx')) %>%
  mutate(date = as.Date(date))



summary(
  comparedf(
    fim_nipa_federal %>%
      mutate(federal_social_benefits = federal_social_benefits + federal_health_outlays),
    bea_federal,
    tol.num.val = 0.1)
)


