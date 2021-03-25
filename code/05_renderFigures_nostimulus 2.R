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
dir.create('results/', thismonth)
## Fiscal Impact
rmarkdown::render('code/Fiscal-Impact-nostimulus.RMD',
                  output_file = here('results',
                                     thismonth,
                                     paste0('Fiscal-Impact-nostimulus', Sys.Date(), '.pdf')
                  )
)

## Fiscal Impact Expanded
rmarkdown::render('code/Fiscal-Impact-Expanded-nostimulus.RMD',
                  output_file = here('results',
                                     thismonth,
                                     paste0('Fiscal-Impact-Expanded-nostimulus',
                                            Sys.Date(),
                                            '.pdf')
                  )
)
