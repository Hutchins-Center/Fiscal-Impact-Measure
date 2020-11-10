library(arsenal)
xx_official <- read_excel("data/results/xx-official.xlsx", na = 'NA') %>%
  mutate(date = as.Date(date)) %>%
  select(-c(gfrptb_g, gsrpt_gdp, gsrpri_gdp, gsrcp_gdp, 
            gsrs_gdp, gfsave, gssave,
            gfrptsave))
xx_temp <- xx %>%
  filter(date >= '2019-03-31' & date <= '2023-12-31') %>%
  select(-c(gfrptCurrentLaw, gfrptCurrentLaw_g))
summary(comparedf(xx_temp, xx_official))

        