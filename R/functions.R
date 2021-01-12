total_fiscal_impact_plot <- function() {
  contributions %>%
    select(date, fiscal_impact, fiscal_impact_moving_average) %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = " Quarterly fiscal impact") +
    scale_fill_manual(labels = " Quarterly fiscal impact",
                      values = total_pink) +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow")
}       

taxes_transfers <- function() {
  contributions %>%
    select(date,  state_local_cont, federal_cont, taxes_transfers_cont, fiscal_impact_moving_average) %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = 'Total') +
    scale_fill_manual(
      labels = c(" State & Local Purchases", " Federal Purchases", " Taxes, Transfers, & Subsidies"),
      values =  c(state_local_purple, federal_blue, taxes_transfers_green)
    ) +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow")
}
components_govt <- function() {
  contributions %>%
    select(date, fiscal_impact_moving_average, state_local_cont, state_taxes_transfers_cont, 
           federal_cont, federal_taxes_transfers_cont)  %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = 'Total') +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow") +
    scale_fill_brewer(labels = c(" State & Local Purchases",
                                 " State & Local Taxes, Transfers, & Subsidies",
                                 " Federal Purchases",
                                 " Federal Taxes, Transfers, & Subsidies")
    )
}  
  taxes_transfers_govt <- function(){
    contributions %>%
      select(date, fiscal_impact_moving_average,
             health_outlays_cont, social_benefits_cont, 
             noncorp_taxes_cont, corporate_taxes_cont,
             purchases_cont, subsidies_cont) %>%
      pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
      fim_plot(title = "Taxes and Transfers Components") +
      ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                        ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow") +
      scale_fill_brewer(labels = c(" Health Outlays", " Social Benefits",
                                   " Noncorporate Taxes", " Corporate Taxes", 
                                   " Purchases", " Subsidies")
      )
  }
  
  taxes <- function(){
    contributions %>%
      filter(date > lubridate::today() - lubridate::years(2)) %>%
      select(date, federal_corporate_taxes_cont, federal_noncorp_taxes_cont,
             state_corporate_taxes_cont, state_noncorp_taxes_cont) %>%
      pivot_longer(cols = -c(date), names_to = 'variable') %>%
      ggplot(aes(x = date,
                 y = value,
                 fill = variable)) + 
      geom_col(position = 'dodge') +
      scale_fill_brewer(labels = c(' Federal Corporate Taxes', ' Federal Non-Corporate Taxes', ' State Corporate Taxes', ' State Non-Corporate Taxes'), type = 'qual', 
                        palette = 'Dark2') +
      fim_theme()+
      labs(
        title = '**Impact of Taxes by Level of Government**'
      )
  }
social_benefits <- function(){  
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(5)) %>%
    select(date, 
           federal_social_benefits_cont, state_social_benefits_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(" Federal", " State"), type = 'seq'
    ) +
    fim_theme() +
    labs(title = 'Impact of Social Benefits',
         x= '',
         y ='')
}

health_outlays <- function(){
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(5)) %>%
    select(date, 
           federal_health_outlays_cont, state_health_outlays_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(" Federal", " State"), type = 'div', direction = -1
    ) +
    fim_theme() +
    labs(title = 'Impact of transfers',
         x= '',
         y ='')
}


legislation <- function(){
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(1)) %>%
    select(date, 
           subsidies_cont, unemployment_insurance_cont, 
           rebate_checks_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(
      ' Subsidies', ' Unemployment Insurance',
      ' Rebate checks'), type = 'qual'
    ) +
    fim_theme() + 
    labs(title = 'Impact of legislation',
         x = '',
         y = '')
}