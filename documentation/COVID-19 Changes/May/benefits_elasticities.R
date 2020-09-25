library(tidyverse)
library(data.table)

ben <- read.csv("auto_benefits.csv", stringsAsFactors = F) %>% 
  mutate(date = as.Date(date, f = "%d/%m/%y")) %>%
  mutate_at(vars(-c(date)), as.numeric) %>% 
  mutate(dunrate = unrate - shift(unrate,1),
         dgdp = gdp - shift(gdp,1),
         statelocal_exmcaid = statelocalbenefits - medicaid_state,
         federal_other = federalsocialbenefits - snap - ui - medicare - ssi,
         ui_exeuc = ui - ui_euc) %>% 
  mutate_at(vars(-c(date, unrate, gdp, dunrate, dgdp)), ~((./shift(.,1))-1)) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., "NaN"))) %>% 
  filter(date > "1979-01-01" & date < "2020-01-01")


fitsUR <- ben %>% 
  pivot_longer(-c(date,dunrate), names_to = "var") %>% 
  group_by(var) %>%
  do(fitUR = coefficients(lm(value~dunrate, .))[2] + 1) %>% 
  unnest()

fitsGDP <- ben %>% 
  pivot_longer(-c(date,dgdp), names_to = "var") %>% 
  group_by(var) %>%
  do(fitUR = coefficients(lm(value~dgdp, .))[2] + 1) %>% 
  unnest()

