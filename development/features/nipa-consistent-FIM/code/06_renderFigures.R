## ---------------------------
##
## Script name: Render Figures
##
## Purpose of script:
## Render R Markdown files
## Author: Manuel Alcalá Kovalski
##
## Date Created: 2020-10-13
##
##

library('here')
thismonth <- format(Sys.Date(), "%m-%Y")
nipa_path <- 'development/features/nipa-consistent-FIM'
dir.create(here(nipa_path, 'results', thismonth))
## Fiscal Impact
rmarkdown::render(here(nipa_path, 'code', 'fim-nipa.RMD'),
                  output_file = here(nipa_path, 
                                     'results',
                                     thismonth,
                                     paste0('Fiscal-Impact-', Sys.Date(), '.pdf')
                                     )
                  )

## Fiscal Impact Expanded
rmarkdown::render(here(nipa_path, 'code', 'fim-nipa-expanded.RMD'),
                  output_file = here(nipa_path,
                                     'results',
                                     thismonth,
                                     paste0('Fiscal-Impact-Expanded-',
                                            Sys.Date(),
                                            '.pdf')
                                     )
)
