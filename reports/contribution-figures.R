

# Packages ------------------------------------------------------------------------------------

library('targets')
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
library('gghutchins')
conflicted::conflict_prefer('filter', 'dplyr')
conflicted::conflict_prefer('geom_col', 'ggplot2')
conflicted::conflict_prefer('geom_line', 'ggplot2')
#Timezone
Sys.setenv(TZ='UTC')
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
      geom_col(position=position_dodge2(reverse = TRUE)) +
      geom_vline(xintercept = last_hist_date - 45, linetype = 'dotted') +
      theme_hutchins() +
      labs(x = '', y = '', title = paste0(title, '<br>')) +
      scale_fill_hutchins(
        name = "",
        labels = c('Updated', 'Previous'),
        pal = 'qual',
        rev = FALSE
      ) +
      scale_x_yearquarter(breaks = waiver(),
                          date_breaks = '3 months',
                          date_labels = "Q%q") +
      facet_grid( ~ year(date),
                  space = "free_x",
                  scales = "free_x",
                  switch = "x")  +
      theme(legend.position = 'top') +
      guides(fill = guide_legend(reverse = TRUE)) 

  }


# Data ----------------------------------------------------------------------------------------

#These are data for contributions, not levels


last_month <- get_previous_month()
current_month <- get_current_month()
#tar_load(last_hist_date)
#tar_load(last_proj_date)
last_hist_date <- lubridate::as_date("2021-03-31")
last_proj_date <- lubridate::as_date("2023-03-31")
old <-
 # read_excel(glue('results/{last_month}/fim-{last_month}.xlsx'), na = "NA") %>%
  read_excel(glue('results/3-2021/fim-3-2021.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  select(date, fiscal_impact, ends_with('cont'), federal_cont_ex_grants) %>% 
  rename_with(.fn =  ~ str_remove(.x, '_cont'), ends_with('cont')) %>%
  mutate(key = 'old',
         date = yearquarter(date)) %>% 
  mutate(federal_unemployment_insurance = federal_unemployment_insurance + federal_ui_arp,  state_unemployment_insurance = state_unemployment_insurance + state_ui_arp)

new <-
 # read_excel(glue('results/{current_month}/fim-{current_month}.xlsx'), na = "NA") %>%
  read_excel(glue('results/4-2021/fim-4-2021.xlsx'), na = "NA") %>%
             
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  select(date, fiscal_impact, ends_with('cont')) %>%
  rename_with(.fn =  ~ str_remove(.x, '_cont'), ends_with('cont')) %>%
  mutate(key = 'new',
         date = yearquarter(date))



# Figures -------------------------------------------------------------------------------------

theme_set(theme_hutchins())
new %>% 
  mutate(arp_cont = federal_ui_arp + rebate_checks_arp + aid_to_small_businesses + health_grants_arp + non_health_grants + other_direct_aid + other_vulnerable) %>% 
  select(date, arp_cont) %>% 
  pivot_longer(arp_cont) %>% 
  ggplot(aes(x = date, y = value, fill = name)) +
  geom_col() +
  labs(title = 'Contribution of the American Rescue Plan',
       subtitle = 'Includes unemployment insurance, rebate checks, aid to small businesses, health grants, grants to S&L governments, direct aid to households and aid to vulnerable individuals')  +
  scale_fill_hutchins()

(fiscal_impact <-
  comparison_plot(title = 'Quarterly Fiscal Impact'))
# Purchases with Grants
## Total   
federal <- comparison_plot(federal, title = 'Federal Purchases with Grants')
federal_new <-comparison_plot(federal_nom_new, title = "Federal Purchases (no grants) New Add Factor")
state <- comparison_plot(state_local, title = 'State Purchases with Grants')

# Purchases
## Total   EDITING HERE
#federal <- comparison_plot(federal_cont_ex_grants, title = 'Federal Purchases')
#state <- comparison_plot(state_local_ex_grants, title = 'State purchases')

## Excluding grants
# federal_nom  <-
#   comparison_plot(federal_nom, title = 'Federal Purchases Without Grants')
# state_nom  <-
#   comparison_plot(state_local_nom, title = 'State Purchases Without Grants')
## Grants
grants <-
  comparison_plot(federal_grants, title = 'Consumption and Investment Grants')
consumption_grants <-
  comparison_plot(federal_cgrants, title = 'Consumption Grants')
investment_grants <-
  comparison_plot(federal_igrants, title = 'Investment Grants')
arp_grants <- comparison_plot(non_health_grants, title = 'ARP Grants')

# Taxes

taxes<- comparison_plot(taxes, 'Taxes')
federal_taxes<- comparison_plot(taxes, 'Federal Taxes')
state_taxes<- comparison_plot(taxes, 'State Taxes')


new %>%
  select(date,federal_taxes) %>%
  pivot_longer(-date) %>%
  ggplot(aes(x=date,y =value,fill=name))+geom_line()
corp_taxes <- comparison_plot(corporate_taxes, 'Taxes')
federal_corp_taxes <- comparison_plot(corporate_taxes, 'Federal Taxes')
state_corp_taxes <- comparison_plot(corporate_taxes, 'State Taxes')

noncorp_taxes <- comparison_plot(noncorp_taxes, 'Taxes')
federal_noncorp_taxes <- comparison_plot(noncorp_taxes, 'Federal Taxes')
state_noncorp_taxes <- comparison_plot(noncorp_taxes, 'State Taxes')

# Transfers
transfers <- comparison_plot(transfers, 'Transfers')
federal_transfers <- comparison_plot(federal_transfers, 'Federal Transfers')
state_transfers <- comparison_plot(state_transfers, 'State Transfers')

# Health outlays
health_outlays <-
  comparison_plot(health_outlays, title = 'Health Outlays')
federal_health_outlays <-
  comparison_plot(federal_health_outlays, title = 'Federal Health Outlays')
state_health_outlays <-
  comparison_plot(state_health_outlays, title = 'State Health Outlays')

# Subsidies
subsidies <- comparison_plot(subsidies, title = 'Subsidies')

# Unemployment Insurance
ui <-
  comparison_plot(unemployment_insurance, title = 'Unemployment Insurance')
federal_ui <-
  comparison_plot(federal_unemployment_insurance, title = 'Federal Unemployment Insurance')
state_ui <-
  comparison_plot(state_unemployment_insurance, title = ' State Unemployment Insurance')

# Rebate checks
rebate_checks <-
  comparison_plot(variable = rebate_checks, title = 'Rebate checks')
# Social benefits
social_benefits <-
  comparison_plot(social_benefits, title = 'Social Benefits Remainder')
federal_social_benefits <-
  comparison_plot(federal_social_benefits, title = 'Federal Social Benefits Remainder')
state_social_benefits <-
  comparison_plot(state_social_benefits, title = 'State Social Benefits Remainder')



# Levels --------------------------------------------------------------------------------------


old <-
 # read_excel(glue('results/{last_month}/fim-{last_month}.xlsx'), na = "NA") %>%
  read_excel(glue('results/3-2021/fim-3-2021.xlsx'), na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>% 
  mutate(key = 'old',
         date = yearquarter(date)) %>%
  mutate(grants = federal_cgrants + federal_igrants,
         federal_purchases = federal_nom + grants,
         state_purchases = state_local_nom - grants,
         taxes = corporate_taxes + noncorp_taxes,
         federal_taxes = federal_corporate_taxes + federal_noncorp_taxes,
         state_taxes = state_corporate_taxes + state_noncorp_taxes) %>%  
  mutate(federal_unemployment_insurance = federal_unemployment_insurance + federal_ui_arp,  state_unemployment_insurance = state_unemployment_insurance + state_ui_arp)


new <-
 # read_excel(glue('results/{current_month}/fim-{current_month}.xlsx'),na = "NA") %>%
  read_excel(glue('results/4-2021/fim-4-2021.xlsx'),na = "NA") %>%
  mutate(date = as_date(date)) %>%
  filter(date >= '2017-12-31' & date <= last_proj_date) %>%
  mutate(key = 'new',
         date = yearquarter(date)) %>%
  mutate(grants = federal_cgrants + federal_igrants,

         federal_purchases = federal_purchases_with_grants,
         state_purchases = state_purchases_with_grants,

         taxes = corporate_taxes + noncorp_taxes,
         federal_taxes = federal_corporate_taxes + federal_noncorp_taxes,
         state_taxes = state_corporate_taxes + state_noncorp_taxes)




# Figures -------------------------------------------------------------------------------------



# Purchases
## Total
federal_levels  <-
  comparison_plot(federal_nom, title = 'Federal Purchases')
state_levels  <-
  comparison_plot(state_purchases, title = 'State Purchases')
old%>% mutate(state_purchases_nipa = state_purchases + federal_cgrants + federal_igrants)
new %>% mutate(state_purchases_nipa = state_purchases + federal_cgrants + federal_igrants) %>%  ggplot(aes(x= date, y= state_purchases_nipa)) + geom_col() 
new %>% mutate(state_purchases_nipa = state_purchases + federal_cgrants + federal_igrants)%>% select(date, state_purchases_nipa) %>% View()

 

#difference_fed_state_levels <-
   # comparison_plot(TO DO)


## Grants
grants_levels  <-
  comparison_plot(grants, title = 'Consumption and Investment Grants')
consumption_grants_levels  <-
  comparison_plot(federal_cgrants, title = 'Consumption Grants')
investment_grants_levels  <-
  comparison_plot(federal_igrants, title = 'Investment Grants')
arp_grants_levels <- 
    comparison_plot(non_health_grants, title = 'ARP Grants')


old <- old %>% mutate(grants_old = grants) 
old_new_combined <- (new,old$grants_old) %>% 
old_new_combined %>%mutate(grants_differences = grants - grants_old) %>% select(date, grants, grants_old, grants_differences) %>% View()

# Taxes
# taxes <- comparison_plot(taxes, 'Taxes')
# federal_taxes <- comparison_plot(taxes, 'Federal Taxes')
# state_taxes <- comparison_plot(taxes, 'State Taxes')

taxes_levels <- comparison_plot(taxes, 'Taxes')
federal_taxes_levels <- comparison_plot(federal_taxes, 'Federal Taxes')
state_taxes_levels <- comparison_plot(taxes, 'State Taxes')

corp_taxes_levels  <- comparison_plot(corporate_taxes, 'Corporate Taxes')
federal_corp_taxes_levels  <- comparison_plot(corporate_taxes, 'Corporate Federal Taxes')
state_corp_taxes_levels  <- comparison_plot(corporate_taxes, 'Corporate State Taxes')

noncorp_taxes_levels  <- comparison_plot(noncorp_taxes, 'Non-Corporate Taxes')
federal_noncorp_taxes_levels  <- comparison_plot(noncorp_taxes, 'Federal Non-Corporate Taxes')
state_noncorp_taxes_levels  <- comparison_plot(noncorp_taxes, 'State Non-Corporate Taxes')


# Transfers
transfers_levels  <- comparison_plot(social_benefits, 'Transfers')
federal_transfers_levels  <- comparison_plot(federal_social_benefits, 'Federal Transfers')
state_transfers_levels  <- comparison_plot(state_social_benefits, 'State Transfers')

# Health outlays
health_outlays_levels  <-
  comparison_plot(health_outlays, title = 'Health Outlays')
federal_health_outlays_levels  <-
  comparison_plot(federal_health_outlays, title = 'Federal Health Outlays')
state_health_outlays_levels  <-
  comparison_plot(state_health_outlays, title = 'State Health Outlays')

# Subsidies
subsidies_levels  <- comparison_plot(subsidies, title = 'Subsidies')


# Unemployment Insurance
ui_levels  <-
    comparison_plot(unemployment_insurance, title = 'Unemployment Insurance')
federal_ui_levels  <-
    comparison_plot(federal_unemployment_insurance, title = 'Federal Unemployment Insurance')
state_ui_levels  <-
    comparison_plot(state_unemployment_insurance, title = ' State Unemployment Insurance')

# Rebate checks
old <- old %>% mutate(rebate_checks = rebate_checks + rebate_checks_arp)

rebate_checks_levels  <-
    comparison_plot(variable = rebate_checks, title = 'Rebate checks')
# Social benefits
social_benefits_levels  <-
    comparison_plot(social_benefits, title = 'Social Benefits Remainder')
federal_social_benefits_levels  <-
    comparison_plot(federal_social_benefits, title = 'Federal Social Benefits Remainder')
state_social_benefits_levels  <-
    comparison_plot(state_social_benefits, title = 'State Social Benefits Remainder')


