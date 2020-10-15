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

## Fiscal Impact
rmarkdown::render('code/Fiscal-Impact.RMD',
                  output_file = here('results',
                                     thismonth,
                                     paste0('Fiscal-Impact-', Sys.Date(), '.pdf')
                                     )
                  )

## Fiscal Impact Expanded
rmarkdown::render('code/Fiscal-Impact-Expanded.RMD',
                  output_file = here('results',
                                     thismonth,
                                     paste0('Fiscal-Impact-Expanded-',
                                            Sys.Date(),
                                            '.pdf')
                                     )
)
