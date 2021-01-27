
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
comparison_theme <- function(){
  theme(plot.title.position = 'plot',
        text = element_text(family = "Roboto", color = "grey20"),
        plot.title = element_textbox_simple(
          family =  'Roboto',
          size = 16,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0)
        ),
        axis.title.x = element_textbox_simple(
          width = NULL,
          padding = margin(4, 4, 4, 4),
          margin = margin(4, 0, 0, 0)
        ),
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


comparison_plot <- function(variable = fiscal_impact, title = '', df = new, df_old = old){
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
    comparison_theme()
}

# Data ----------------------------------------------------------------------------------------


loadd(last_hist_date)
last_proj_date <- last_hist_date + years(2)


last_month <- get_previous_month()
current_month <- get_current_month()

old <- 
  read_excel(glue('results/{last_month}/fim-{last_month}.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  select(date, fiscal_impact, ends_with('cont')) %>%
  rename_with(.fn =~ str_remove(.x, '_cont'), ends_with('cont')) %>%
  mutate(key = 'old',
         date = yearquarter(date)) 



new <- read_excel(glue('results/{current_month}/fim-{current_month}.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  select(date, fiscal_impact, ends_with('cont')) %>%
  rename_with(.fn =~ str_remove(.x, '_cont'), ends_with('cont')) %>%
  mutate(key = 'new',
       date = yearquarter(date)) 



# Figures -------------------------------------------------------------------------------------


fiscal_impact <- comparison_plot(title = 'Quarterly Fiscal Impact')  
federal <- comparison_plot(federal, title = 'Federal Purchases')
federal_nom  <- comparison_plot(federal_nom, title = 'Federal Purchases Minus Grants')
federal_grants <- comparison_plot(federal_grants, title = 'Federal Consumption and Investment Grants')
state_nom  <- comparison_plot(state_local_nom, title = 'State Purchases')
social_benefits <-  comparison_plot(social_benefits, title = 'Social Benefits')
federal_social_benefits <-comparison_plot(federal_social_benefits, title = 'Federal Social Benefits')
state_social_benefits <-  comparison_plot(state_social_benefits, title = 'State Social Benefits')
health_outlays <- comparison_plot(health_outlays, title = 'Health Outlays')
federal_health_outlays <- comparison_plot(federal_health_outlays, title = 'Federal Health Outlays')
state_health_outlays <- comparison_plot(state_health_outlays, title = 'State Health Outlays')
subsidies <- comparison_plot(subsidies, title = 'Subsidies')
ui <- comparison_plot(unemployment_insurance, title = 'Unemployment Insurance')
federal_ui <- comparison_plot(federal_unemployment_insurance, title = 'Federal Unemployment Insurance')
state_ui <- comparison_plot(state_unemployment_insurance, title = ' State Unemployment Insurance')

rebate_checks <- comparison_plot(variable = rebate_checks,title = 'Rebate checks')

transfers <- comparison_plot(transfers, 'Transfers')
taxes <- comparison_plot(taxes, 'Taxes')
new %<>%
  mutate(transfers_net_taxes = transfers - taxes)
old %<>%
  mutate(transfers_net_taxes = taxes_transfers)
transfers_net_taxes <- comparison_plot(transfers_net_taxes)
federal_taxes <- comparison_plot(federal_taxes, 'Taxes')
state_taxes <- comparison_plot(state_taxes, 'Taxes')


corporate_taxes <- comparison_plot(corporate)
noncorp_taxes <- comparison_plot(noncorp)



