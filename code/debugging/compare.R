library(arsenal)
xx_official <- read_excel("data/results/xx-official.xlsx", na = 'NA') %>%
  mutate(date = as.Date(date)) %>%
  select(-c(gsrpt_gdp, gsrpri_gdp, gsrcp_gdp, 
            gsrs_gdp, gfsave, gssave,
            gfrptsave, yptxb, cpiu_g.x, cpiu_g.y)) 
xx_temp <- xx %>%
  filter(date >= '2019-03-31' & date <= '2023-12-31') %>%
  rename(gfrptb_g = gfrptCurrentLaw_g) %>%
  select(-c(gfrptCurrentLaw, cpiu_g.x, cpiu_g.y))
write_xlsx(xx_temp, 'data/results/xx-temp.xlsx')
summary(comparedf(xx_temp, xx_official))

fim_official <-
  read_excel('data/results/fim-official.xlsx', na = 'NA') %>%
  mutate(date = as.Date(date)) %>%
  filter(date > '2015-12-31' & date <= '2022-09-30') %>%
  select(-starts_with('add'))

fim_temp <-
  fim %>%
  filter(date > '2015-12-31') %>%
  select(-starts_with('add'))

add_factors_official <- fim_official %>%
  select(date, starts_with('add')) %>%
  filter(date >= '2020-09-30')
add_factors_temp <-
  fim_temp %>%
  select(date, starts_with('add')) %>%
  filter(date >= '2020-09-30')
summary(comparedf(fim_official, 
                  fim_temp))
summary(comparedf(add_factors_official, add_factors_temp))        
        