source('src/packages.R')
library('glue')

last_hist_date <- as.Date("2020-09-30")
last_proj_date <- last_hist_date + years(2) + months(7)


update_month <- month(Sys.Date())
update_year <- year(Sys.Date())
update_path <- glue::glue('results/{ update_month }-{update_year }')

# update <- read_excel(glue('{update_path}/fim-{update_month}-{update_year}.xlsx'), na = "NA")
stimulus <- read_excel(glue('{update_path}/fim_interactive.xlsx'), na = "NA") %>%
  filter(year >= '2017')

no_stimulus <- read_excel(glue('{update_path}/no-stimulus/fim_interactive_nostimulus.xlsx'), na = "NA") %>%
  filter(year >= '2017') 

write_xlsx(stimulus, glue('{update_path}/fim_stimulus.xlsx'))
write_xlsx(no_stimulus, glue('{update_path}/fim_nostimulus.xlsx'))


# Net MPC -------------------------------------------------------------------------------------

stimulus <-
  read_excel(glue('{update_path}/fim.xlsx'), na = "NA") %>%
  filter(date > '2016-12-31') %>%
  select(date, ends_with('net'), ends_with('xmpc')) %>%
  mutate(date = as.Date(date))

no_stimulus <- 
  read_excel(glue('{update_path}/no-stimulus/fim_nostimulus.xlsx'), na = "NA") %>%
  filter(date > '2016-12-31') %>%
  select(date, ends_with('net'), ends_with('xmpc')) %>%
  mutate(date = as.Date(date))

write_xlsx(stimulus, glue('{update_path}/fim_stimulus_netc.xlsx'))
write_xlsx(no_stimulus, glue('{update_path}/fim_nostimulus_net.xlsx'))

purchases_diff <-
  stimulus$federal_nom - no_stimulus$federal_nom
