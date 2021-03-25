source('src/packages.R')
library('glue')

last_hist_date <- as.Date("2020-09-30")
last_proj_date <- last_hist_date + years(2) + months(7)

previous_month <- month(Sys.Date() - months(1))
previous_year <- year(Sys.Date() - months(1)) 
previous_path <- glue::glue('results/{ previous_month }-{previous_year }')

previous <- read_excel(glue('{previous_path}/fim.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  select(-c(contains('add'), contains('override'))) %>%
  select(-contains('cont')) %>%
  select(-contains('net')) 

update_month <- month(Sys.Date())
update_year <- year(Sys.Date())
update_path <- glue::glue('results/{ update_month }-{update_year }')

# update <- read_excel(glue('{update_path}/fim-{update_month}-{update_year}.xlsx'), na = "NA")
update <- read_excel(glue('{update_path}/fim.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  select(-c(contains('add'), contains('override'))) %>%
  select(-contains('cont')) %>%
  select(-contains('net')) %>%
  select(-gtfp)

write_xlsx(previous, 'reports/nov-fim.xlsx')
write_xlsx(update, 'reports/dec-fim.xlsx')



