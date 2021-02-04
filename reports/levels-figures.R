

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
comparison_theme <- function() {
  theme(
    plot.title.position = 'plot',
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


comparison_plot <-
  function(variable = fiscal_impact,
           title = '',
           df = new,
           df_old = old) {
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
      scale_fill_brewer(
        name = "",
        labels = c('Updated', 'Previous'),
        type = 'qual',
        palette = 'Paired',
        direction = -1
      ) +
      scale_x_yearquarter(breaks = waiver(),
                          date_breaks = '3 months',
                          date_labels = "Q%q") +
      facet_grid( ~ year(date),
                  space = "free_x",
                  scales = "free_x",
                  switch = "x")  +
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
  mutate(key = 'old',
         date = yearquarter(date)) %>%
  mutate(grants = federal_cgrants + federal_igrants,
         federal_purchases = federal_nom + grants)


new <-
  read_excel(glue('results/{current_month}/fim-{current_month}.xlsx'),
             na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  mutate(key = 'new',
         date = yearquarter(date)) %>%
  mutate(grants = federal_cgrants + federal_igrants,
         federal_purchases = federal_nom + grants)




# Figures -------------------------------------------------------------------------------------



# Purchases
## Total
(federal <- comparison_plot(federal_purchases, title = 'Federal Purchases'))

## Excluding grants
federal_nom  <-
  comparison_plot(federal_nom, title = 'Federal Purchases Without Grants')
state_nom  <-
  comparison_plot(state_local_nom, title = 'State Purchases Without Grants')
## Grants
grants <-
  comparison_plot(grants, title = 'Consumption and Investment Grants')
consumption_grants <-
  comparison_plot(federal_cgrants, title = 'Consumption Grants')
investment_grants <-
  comparison_plot(federal_igrants, title = 'Investment Grants')

federal_nom
state_nom
grants
consumption_grants
investment_grants

# Taxes
# taxes <- comparison_plot(taxes, 'Taxes')
# federal_taxes <- comparison_plot(taxes, 'Federal Taxes')
# state_taxes <- comparison_plot(taxes, 'State Taxes')

corp_taxes <- comparison_plot(corporate_taxes, 'Taxes')
federal_corp_taxes <- comparison_plot(corporate_taxes, 'Federal Taxes')
state_corp_taxes <- comparison_plot(corporate_taxes, 'State Taxes')

noncorp_taxes <- comparison_plot(noncorp_taxes, 'Taxes')
federal_noncorp_taxes <- comparison_plot(noncorp_taxes, 'Federal Taxes')
state_noncorp_taxes <- comparison_plot(noncorp_taxes, 'State Taxes')

corp_taxes
federal_corp_taxes
state_corp_taxes

noncorp_taxes
federal_noncorp_taxes
state_noncorp_taxes


# Transfers
transfers <- comparison_plot(social_benefits, 'Transfers')
federal_transfers <- comparison_plot(federal_social_benefits, 'Federal Transfers')
state_transfers <- comparison_plot(state_social_benefits, 'State Transfers')

transfers
federal_transfers
state_transfers

# Health outlays
health_outlays <-
  comparison_plot(health_outlays, title = 'Health Outlays')
federal_health_outlays <-
  comparison_plot(federal_health_outlays, title = 'Federal Health Outlays')
state_health_outlays <-
  comparison_plot(state_health_outlays, title = 'State Health Outlays')

health_outlays
federal_health_outlays
state_health_outlays

# Subsidies
(subsidies <- comparison_plot(subsidies, title = 'Subsidies'))


# Unemployment Insurance
(ui <-
  comparison_plot(unemployment_insurance, title = 'Unemployment Insurance'))
(federal_ui <-
  comparison_plot(federal_unemployment_insurance, title = 'Federal Unemployment Insurance'))
(state_ui <-
  comparison_plot(state_unemployment_insurance, title = ' State Unemployment Insurance'))

# Rebate checks
(rebate_checks <-
  comparison_plot(variable = rebate_checks, title = 'Rebate checks'))
# Social benefits
(social_benefits <-
  comparison_plot(social_benefits, title = 'Social Benefits Remainder'))
(federal_social_benefits <-
  comparison_plot(federal_social_benefits, title = 'Federal Social Benefits Remainder'))
(state_social_benefits <-
  comparison_plot(state_social_benefits, title = 'State Social Benefits Remainder'))

