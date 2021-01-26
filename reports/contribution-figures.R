
# Packages ------------------------------------------------------------------------------------

library('drake')
library('tidyverse')
library('magrittr')
library('ggthemes')
library('ggtext')
library('ggthemes')
library('gridExtra')
library('fim')
library('lubridate')
library('glue')
library('readxl')
library('tsibble')
# Functions -----------------------------------------------------------------------------------
custom_theme <- function(){
  theme(plot.title.position = 'plot',
        plot.title = element_textbox_simple(),
        plot.caption = element_textbox_simple(),
        strip.placement = 'outside',
        strip.background = element_blank(),
        panel.spacing = unit(0, 'lines'),
        legend.position = 'bottom',
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        plot.subtitle = element_textbox_simple(size = 10) 
  )
}
comparison_plot <- function(df = new, df_old = old, variable = fiscal_impact, title = ''){
  variable <- enquo(variable)
  df %>%
    bind_rows(df_old) %>%
    select(date, !!variable, key) %>%
    group_by(key) %>%
    pivot_longer(where(is.numeric)) %>%
    ggplot(aes(x = date,
               y = value,
               fill = key)) +
    geom_col(position = 'dodge') +
    geom_vline(xintercept = last_hist_date - 45, linetype = 'dotted') +
    labs(x = '', y = '', title = title) +
    scale_fill_brewer(name = "", labels = c('Updated', 'Previous'),
                      type = 'qual', palette = 'Paired', direction = -1) + 
    scale_x_yearquarter(breaks = waiver(),
                        date_breaks = '3 months',
                        date_labels = "Q%q") +
    facet_grid(~ year(date), 
               space="free_x", 
               scales="free_x", 
               switch="x")  +
    theme_hc() +
    custom_theme()
}

# Data ----------------------------------------------------------------------------------------


loadd(last_hist_date)
loadd(last_proj_date)

last_month <- get_previous_month()
current_month <- get_current_month()

old <- 
  read_excel(glue('results/{last_month}/fim-{last_month}.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  mutate(key = 'old',
         date = yearquarter(date)) 

new <- read_excel(glue('results/{current_month}/fim-{current_month}.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  mutate(key = 'new',
         date = yearquarter(date)) 



# Figures -------------------------------------------------------------------------------------


fiscal_impact <- comparison_plot(title = 'Quarterly Fiscal Impact')  
federal_unemployment_insurance <- comparison_plot(variable = rebate_checks_cont,title = 'Rebate checks')
rebate_checks <- comparison_plot(variable = rebate_checks_cont,title = 'Rebate checks')
