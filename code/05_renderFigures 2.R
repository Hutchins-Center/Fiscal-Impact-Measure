## -------------------------------
## Render R Markdown files
## -------------------------------

library('here')
library('glue')
current_month <- format(Sys.Date(), "%m-%Y")
dir.create(glue('results/{current_month}'))
## Fiscal Impact
rmarkdown::render('code/Fiscal-Impact.RMD',
                  output_file = here('results',
                                     current_month,
                                     glue('Fiscal-Impact-{current_month}.pdf')
                                     )
                  )

## Fiscal Impact Expanded
rmarkdown::render('code/Fiscal-Impact-Expanded.RMD',
                  output_file = here('results',
                                     current_month,
                                     glue('Fiscal-Impact-Expanded-{current_month}.pdf')
                                     ),
                  intermediates_dir = here('results', current_month),
                  clean = TRUE
)
