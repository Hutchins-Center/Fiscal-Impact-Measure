library('tidyverse')
interactive <- readxl::read_xlsx('results/4-2021/fim-4-2021.xlsx') %>% 
  fim::prepare_interactive() %>% filter(year > 2018)  


contributions <- readxl::read_xlsx('results/4-2021/fim-4-2021.xlsx') %>% 
  select(date, ends_with('cont')) %>% 
  mutate(date = yearquarter(lubridate::as_date(date)))  %>% 
  filter(date  >=  yearquarter('2019 Q1'))


summary <-  contributions %>% 
  transmute(date,
   
         federal_purchases = federal_nom_cont,
         state_purchases = state_local_nom_cont,
         grants_cont = federal_cgrants_cont + federal_igrants_cont,
         
         federal_purchases_after_grants_cont = federal_nom_cont + grants_cont,
         state_purchases_after_grants_cont = state_local_nom_cont  - grants_cont,
         total_transfers_cont = federal_transfers_cont + state_transfers_cont,
         federal_transfers_cont,
         federal_social_benefits_without_ui_or_rebate_contribution = federal_social_benefits_cont,
         rebate_checks_cont,
         federal_unemployment_insurance_cont,
         federal_health_outlays_cont,
         federal_subsidies_cont,
         state_transfers_cont, 
         state_social_benefits_without_ui_contribution = state_social_benefits_cont,
         state_unemployment_insurance_cont,
         state_health_outlays_cont,
         state_subsidies_cont,
         taxes_cont = federal_taxes_cont + state_taxes_cont,
         federal_taxes_cont,
         state_taxes_cont) %>% 
  rename_with(.fn = ~snakecase::to_title_case(.),
              .cols = everything()) %>% 
  mutate(across(where(is.numeric),
                ~ . / 100))

library(openxlsx)

wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 14)



addWorksheet(wb, sheetName = "Summary of contributions", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, sheet = 1, x = summary,
               colNames = TRUE, rowNames = FALSE,
               tableStyle = "TableStyleLight9")

setColWidths(wb, sheet = 1, cols = "A", widths = 18)

saveWorkbook(wb, "results/4-2021/summary.xlsx", overwrite = TRUE) #
