library("r2excel")
library('snakecase')
loadd(contributions)
contributions %<>% 
  mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
  mutate(date = yearquarter(date)) %>%
  column_to_rownames('date') %>%
  rename_with(.fn = ~to_title_case(.),
              .cols = everything())
# Create an Excel workbook. 
# Both .xls and .xlsx file formats can be used.
# Create an Excel workbook. 
# Both .xls and .xlsx file formats can be used.
filename <- "fim-r2xl-example.xlsx"
wb <- createWorkbook(type="xlsx")
# Create a sheet in that workbook to contain the data table
sheet <- createSheet(wb, sheetName = "contributions")

# Add header
xlsx.addHeader(wb, sheet, value="Fiscal Impact Measure Contributions",level=1, 
               color="black", underline=1)
xlsx.addLineBreak(sheet, 2)


# Add table : add a data frame
xlsx.addTable(wb, sheet, data= head(contributions),
              fontColor="darkblue", fontSize=14,
              rowFill=c("white", "lightblue")
)



full_sheet <- createSheet(wb, sheetName = 'total')
xlsx.addHeader(wb, full_sheet, value="Fiscal Impact Measure Contributions",level=1, 
               color="black", underline=1)
xlsx.addTable(wb, full_sheet, data= contributions,
              fontColor="darkblue", fontSize=14,
              rowFill=c("white", "lightblue")
)

no_format <- createSheet(wb, sheetName = 'no-format')
xlsx.addTable(wb, no_format, data = readd(contributions))
# save the workbook to an Excel file




contributions %<>%
  rownames_to_column('date') %>%
  select(date, everything())%>%
  pivot_longer(-date) %>%
  pivot_wider(names_from =  date,values_from = value) 

pivoted <- createSheet(wb, sheetName = 'pivoted')
xlsx.addTable(wb, full_sheet, data= contributions,
              fontColor="darkblue", fontSize=14,
              rowFill=c("white", "lightblue")
)
# save the workbook to an Excel file
saveWorkbook(wb, filename)
xlsx.openFile(filename)# View the file

  
