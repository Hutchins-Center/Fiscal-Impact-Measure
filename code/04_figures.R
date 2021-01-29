
# Packages ------------------------------------------------------------------------------------
source('R/packages.R')
library('lubridate')

# CONSTANTS --------------------------------------------------------------------------------------
start <- as_date("2000-01-01")
end <- as_date("2022-12-31")
last_hist_date <- end - years(2)
end_date_reccession <- last_hist_date + months(1)

# Load data -----------------------------------------------------------------------------------
read_contributions <- function(){
  start <- lubridate::as_date("2000-01-01")
  end <- lubridate::as_date("2022-12-31")
  
  current_month <- glue('{month(today())}-{year(today())}')

    readxl::read_xlsx(glue("results/{current_month}/fim-{current_month}.xlsx")) %>%
    select(date, fiscal_impact, fiscal_impact_moving_average,
           ends_with('cont'), recession) %>%
    mutate(date = as_date(date)) %>% 
    filter(date > start & date <= end)
}

contributions <-
  read_contributions()

  max_y <-
    contributions %>%
    select(fiscal_impact) %>%
    max() %>%
    ceiling() + 1
  min_y <-
    contributions %>%
    select(fiscal_impact:subsidies_cont) %>%
    min() %>%
    floor() - 1


# Theme --------------------------------------------------------------------------------------
# set plot formats, theme 
uni.theme <- function() {
  theme_bw() +
    theme(legend.position = "bottom", 
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.margin=unit(c(1.2,.5,.5,.5),"cm"),
          plot.title = element_markdown(size=12),
          plot.subtitle = element_markdown(size=10) , 
          plot.caption = 
            element_textbox_simple(size = 9,
                                   lineheight = 1,
                                   padding = margin(5.5, 5.5, 5.5, 5.5),
                                   margin = margin(0, 0, 5.5, 0)),
          legend.text=element_markdown(size=10), 
          legend.title=element_blank(),
          legend.spacing.y = unit(2, 'cm')
    ) # , legend.margin = unit(c(rep(-.8, 4)),"cm")
}

## Legend formatting -------------------------------------------------------------------------
guidez <- guides(
fill = guide_legend(keywidth = unit(0.8, "cm"),
                    keyheight = unit(0.4, "cm"), 
                    ncol = 1),
colour = guide_legend(keywidth = unit(0.8, "cm"),
                      keyheight = unit(0.05, "cm"), 
                      ncol = 1)
)


total_pink <- rgb(231, 97, 159, maxColorValue = 255)
state_local_purple = rgb(174, 104, 169,  maxColorValue = 255)
federal_blue = rgb(33, 152, 199,  maxColorValue = 255)
taxes_transfers_green = rgb(27, 149, 83,  maxColorValue = 255)
# Shading -------------------------------------------------------------------------------------
economic_statistics <- 
  readxl::read_excel(here('data', 'raw', 'haver', 'economic_statistics.xlsx')) %>%
  select(date, recessq) %>%
  mutate(date = as_date(date))
recessions <-
  contributions %>% 
  select(date, recession) %>%
  mutate(recession = if_else(is.na(recession),
                             0,
                             recession),
         recession_event = recession - lag(recession),
         start = if_else(recession_event == 1, 
                         date,
                         NA_Date_) ,
         end = if_else(recession_event == -1,
                       date,
                       NA_Date_) 
         ) %>%
  select(start, end) %>%
  pivot_longer(cols = c(start, end)) %>%
  na.omit() %>%
  group_by(name) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  select(-row)
recessions[3,'end'] <- as_date('2021-01-30')
recession_shade <-
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf),
            fill = 'grey', alpha = 0.3)


# Functions -----------------------------------------------------------------------------------
fim_plot <-
  function(df, title){
    df %>%
      ggplot() +
      geom_bar(aes(x = date, y = value, fill = variable),
               stat = 'identity', width = 50) +
      geom_line(
        aes(x = date,
            y = fiscal_impact_moving_average,
            colour = "4-quarter moving-average")
      ) +
      geom_point(
        aes(x = date,
            y = fiscal_impact_moving_average,
            colour = "4-quarter moving-average"), size = 1
      ) +
      labs(
        title = glue("**Hutchins Center Fiscal Impact Measure: {title}**"),
        x = '',
        y = '',
        subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
        caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
      geom_richtext(aes(x = Sys.Date()+350,
                    y = max_y), 
                label = "Projection",
                cex = 2, 
                fill = NA, label.color = NA, # remove background and outline
      ) +
      annotate("rect", xmin = last_hist_date + 10, xmax = end,
               ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
      geom_rect(data = recessions,
                aes(x = NULL, y = NULL,
                    xmin = start, xmax = end, 
                    ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.3) +
      scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = '%Y',
                   expand = c(0,0)) + 
      scale_color_manual(" ", 
                         values=c("4-quarter moving-average" ="black",
                                  "4-quarter moving-average" ="black")) +
      guidez +
      uni.theme() 
  }

# Figures -------------------------------------------------------------------------------------
## Regular -----------------------------------------------------------------------------------
total <-
  contributions %>%
  select(date, fiscal_impact, fiscal_impact_moving_average) %>%
  pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
  fim_plot(title = 'Total') +
  scale_fill_manual(labels = " Quarterly fiscal impact",
                    values = total_pink)
 
components <-
  contributions %>%
  select(date, state_local_cont, federal_cont, taxes_transfers_cont, fiscal_impact_moving_average) %>%
  pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
  fim_plot(title = 'Components') +
  scale_fill_manual(
    labels = c(" State & Local Purchases", " Federal Purchases", " Taxes, Transfers, & Subsidies"),
                values =  c(state_local_purple, federal_blue, taxes_transfers_green)
    )
## Expanded ----------------------------------------------------------------------------------
components_govt <-
  contributions %>%
    select(date, fiscal_impact_moving_average, state_local_cont, state_taxes_transfers_cont, 
           federal_cont, federal_taxes_transfers_cont) %>%
  pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
  fim_plot(title = "Components by Government") +
  scale_fill_brewer(labels = c(" State & Local Purchases",
                               " State & Local Taxes, Transfers, & Subsidies",
                                " Federal Purchases",
                               " Federal Taxes, Transfers, & Subsidies")
                    )
taxes_transfers <-
  contributions %>%
  select(date, fiscal_impact_moving_average,
         health_outlays_cont, social_benefits_cont, 
         noncorp_taxes_cont, corporate_taxes_cont,
         purchases_cont, subsidies_cont) %>%
  pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
  fim_plot(title = "Taxes and Transfers Components") +
  scale_fill_brewer(labels = c(" Health Outlays", " Social Benefits",
                              " Noncorporate Taxes", " Corporate Taxes", 
                              " Purchases", " Subsidies")
  )


