millions_to_billions <- function(df){
  df %>% 
    dplyr::mutate(
      dplyr::across(.cols = any_of(c('gftfbusx', 'gfeghhx', 'gfeghdx', 'gfeigx')),
                    .fns = ~ . / 1000)
    )
}


create_override <- function(df, var, start, end, values){
  override <- 
    tibble(date = df %>%
             filter(date >= start & date <= end) %>%
             pull(date),
           '{{var}}' := values
    )
  df %>%
    rows_update(override, by = 'date')
}

create_projections <- function(df){
components <- get_components_names()
  df %>%  
    make_cumulative_growth_rates() %>%
    fill(components) %>%
    make_forecasts() %>% 
    sum_projections(gtfp, gftfp, gstfp) %>%
    sum_projections(yptx, gfrpt, gsrpt) %>%
    sum_projections(ytpi, gfrpri, gsrpri) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(yctlg, gfrcp, gsrcp) %>%
    sum_projections(gsub, gfsub, gssub)
}

components_growth_rates <- function(df){
  df %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth() 
}

fim_calculations <- function(df){
  df %>%  
    override_projections() %>%
    fill_overrides() %>%
    contributions_purchases_grants() %>%
    total_purchases() %>%
    remove_social_benefit_components() %>%
    taxes_transfers_minus_neutral() %>%
    calculate_mpc('subsidies') %>%
    calculate_mpc('health_outlays') %>%
    calculate_mpc('social_benefits') %>%
    calculate_mpc('unemployment_insurance') %>%
    calculate_mpc('rebate_checks') %>%
    calculate_mpc('noncorp_taxes') %>%
    calculate_mpc('corporate_taxes') %>%
    taxes_contributions() %>%
    sum_taxes_contributions() %>%
    transfers_contributions() %>%
    sum_transfers_contributions() %>%
    sum_taxes_transfers() %>%
    add_social_benefit_components() %>%
    get_fiscal_impact() %>%
    arrange(date, recession, fiscal_impact, fiscal_impact_moving_average, 
            federal_cont, state_local_cont, 
            taxes_transfers_cont, federal_taxes_transfers_cont, state_taxes_transfers_cont)

}



taxes_transfers_minus_zero <- function(df){
  df %>% 
    dplyr::mutate(dplyr::across(.cols = all_of(all_taxes_transfers()),
                                .fns = ~. -dplyr::lag(.) * (1 + pi_pce),
                  .names = ''))
}

contribution_zero <- function(.data, var){
  var <- ensym(var) # quote expression
  var_string <- rlang::as_string(enexpr(var)) # convert quoted expression to string
  deflator_string <- paste0(var_string, "_pi") # create string with the name of relevant deflator
  deflator <- rlang::sym(deflator_string) # convert deflator string to symbol
  
  ## Calculate contribution
  .data %>%
    mutate(
      "{{ var }}_cont" := 400 * ({{ var }} - (1  + !!(deflator) ) * lag({{ var }}) ) / lag(gdp)
    ) %>%
    select(date, !!paste0(var_string, "_cont"))
}

contributions_purchases_grants_zero <- function (df){
  map(alist(federal_nom, state_local_nom, federal_cgrants, 
            federal_igrants), ~contribution_zero(df, !!.x)) %>% reduce(left_join) %>% 
    left_join(df, .)
}

all_taxes_transfers <- function(){
  taxes_transfers <- c("subsidies","health_outlays", "social_benefits",
                       "noncorp_taxes", "corporate_taxes", 'rebate_checks', 
                       'unemployment_insurance')
  government_level <- c('federal', 'state')
  all_taxes_transfers <- c(glue::glue('{taxes_transfers}'), glue::glue('federal_{taxes_transfers}'),
                           glue::glue('state_{taxes_transfers}'))
  return(all_taxes_transfers)
}


add_factors_v2 <- function(df, last_date){
  #load add factor file
  add_factors <- readxl::read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                                    sheet = "FIM Add Factors") %>%
    mutate(
      date = lubridate::as_date(date)
    ) 
  df %>% 
    dplyr::full_join(add_factors %>% dplyr::select(-tidyselect::ends_with('override')) %>% 
                       filter(date > last_date),
                     by = "date") %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::starts_with('add_'),
      .fns = ~ if_else(is.na(.x), 
                       0,
                       .x)
    )
    ) %>%
    dplyr::mutate(
      #calculate new variables by adding the add factors
      state_health_outlays  = state_health_outlays + add_state_health_outlays,
      state_social_benefits  = state_social_benefits + add_state_social_benefits,
      state_noncorp_taxes  =  state_noncorp_taxes + add_state_noncorp_taxes,
      state_corporate_taxes  = state_corporate_taxes + add_state_corporate_taxes,
      
      federal_health_outlays  = federal_health_outlays + add_federal_health_outlays,
      federal_social_benefits  = federal_social_benefits + add_federal_social_benefits,
      # federal_noncorp_taxes  = federal_noncorp_taxes + add_federal_noncorp_taxes,
      # federal_corporate_taxes  = federal_corporate_taxes + add_federal_corporate_taxes,
      federal_subsidies  = federal_subsidies + add_federal_subsidies,
      federal_cgrants = federal_cgrants + add_federal_cgrants,
      
      #new category totals
      health_outlays  = state_health_outlays  + federal_health_outlays ,
      social_benefits  = state_social_benefits  + federal_social_benefits ,
      noncorp_taxes  = state_noncorp_taxes  + federal_noncorp_taxes ,
      corporate_taxes  = state_corporate_taxes  + federal_corporate_taxes ,
      subsidies   = state_subsidies + federal_subsidies,
      state_local_nom = state_local_nom + add_state_purchases,
      federal_nom = add_federal_purchases + federal_nom,
      federal_rebate_checks = federal_rebate_checks + add_rebate_checks,
      rebate_checks = rebate_checks + add_rebate_checks
    )
}

as_state<- function(x){
  x <- glue('state_{x}')
  return(x)
}

as_federal<-function(x){
  x <- glue('federal_{x}')
  return(x)
}
add_factors <- function(df) {
  add_factors <- readxl::read_excel("data/add-ons/add_factors.xlsx",
                                    sheet = "FIM Add Factors") %>% mutate(date = lubridate::as_date(date))
  df %>% dplyr::full_join(
    add_factors %>% dplyr::select(-tidyselect::ends_with("override")) %>%
      filter(date > '2020-12-31'),
    by = "date"
  ) %>% dplyr::mutate(dplyr::across(
    .cols = tidyselect::starts_with("add_"),
    .fns = ~
      if_else(is.na(.x), 0, .x)
  )) %>% dplyr::mutate(
    state_health_outlays = state_health_outlays +
      add_state_health_outlays,
    state_social_benefits = state_social_benefits +
      add_state_social_benefits,
    federal_health_outlays = federal_health_outlays +
      add_federal_health_outlays,
    federal_social_benefits = federal_social_benefits +
      add_federal_social_benefits,
    federal_subsidies = federal_subsidies +
      add_federal_subsidies,
    federal_cgrants = federal_cgrants +
      add_federal_cgrants,
    state_local_nom = state_local_nom +
      add_state_purchases,
    federal_nom = add_federal_purchases +
      federal_nom,
    health_outlays = state_health_outlays +
      federal_health_outlays,
    social_benefits = state_social_benefits +
      federal_social_benefits,
    subsidies = state_subsidies +
      federal_subsidies,
    federal_rebate_checks = federal_rebate_checks +
      add_rebate_checks,
    rebate_checks = rebate_checks + add_rebate_checks
  )
}


fim_plot <- function (df, title, last_date) 
{
  df %>% ggplot() + geom_bar(aes(x = date, y = value, fill = variable), 
                             stat = "identity", width = 50) + geom_line(aes(x = date, 
                                                                            y = fiscal_impact_moving_average, colour = "4-quarter moving-average")) + 
    geom_point(aes(x = date, y = fiscal_impact_moving_average, 
                   colour = "4-quarter moving-average"), size = 1) + 
    labs(title = glue("**Hutchins Center Fiscal Impact Measure: {title}**"), 
         x = "", y = "", subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
         caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis \n        and Congressional Budget Office data; grey shaded areas indicate recessions \n        and yellow shaded areas indicate projection.") + 
    geom_richtext(aes(x = Sys.Date() + 350, y = 16), label = "Projection", 
                  cex = 2, fill = NA, label.color = NA, ) + annotate("rect", 
                                                                     xmin = last_date + 40, xmax = lubridate::as_date("2022-12-31"), 
                                                                     ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow") + 
    scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = "%Y", 
                 expand = c(0, 0)) + scale_color_manual(" ", values = c(`4-quarter moving-average` = "black", 
                                                                        `4-quarter moving-average` = "black")) + uni.theme()
}

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


annual_to_quarter <- function(df) {
  year <-
    df %>%
    tsibble::index_var()
  min <-
    df %>%
    select(rlang::enexpr(year)) %>%
    min()
  
  max <-
    df %>%
    select(rlang::enexpr(year)) %>%
    max()
  start <- tsibble::yearquarter(glue::glue('{min} Q1'))
  end <- tsibble::yearquarter(glue::glue('{max} Q4'))
  x <- seq(start,  end, by = 1)
  
  df %>%
    as_tibble() %>%
    slice(rep(1:n(), each = 4)) %>%
    mutate(date = tsibble::yearquarter(x, fiscal_start =  1)) %>%
    relocate(date, .before =  everything()) %>%
    tsibble::as_tsibble(index = date)
}



transfers_contributions <- function (df)
{
  transfers <- c(
    "social_benefits",
    "health_outlays",
    "subsidies",
    "unemployment_insurance",
    "rebate_checks") %>% paste0("_post_mpc")
  
  arp_transfers <- c('federal_ui_arp',
                     'state_ui_arp',
                     'rebate_checks_arp',
                     'other_direct_aid',
                     'other_vulnerable',
                     'aid_to_small_businesses',
                     'health_grants_arp') %>%  paste0('_minus_neutral_post_mpc')
  all_transfers <-
    c(glue("{transfers}"),
      glue("federal_{transfers}"),
      glue("state_{transfers}"))
  df %>% mutate(across(
    .cols = all_of(all_transfers),
    .fns = ~ 400 *
      .x / lag(gdp),
    .names = "{.col}_cont"
  )) %>% rename_with( ~ gsub("post_mpc_cont",
                             "cont", .x)) %>% 
    mutate(across(.cols = all_of(arp_transfers), 
                  .fns = ~ 400 * .x / lag(gdp),
                  .names = '{.col}_cont')) %>% 
    rename_with(~gsub('minus_neutral_post_mpc_cont', 
                      'cont', .x))
}



sum_transfers_contributions <-function (df){
  transfers <- c(
    "social_benefits",
    "health_outlays",
    "subsidies",
    "unemployment_insurance",
    "rebate_checks"
  )
  df %>% mutate(
    transfers_cont = rowSums(select(., .dots = all_of(
      str_glue("{transfers}_cont")
    )),
    na.rm = TRUE),
    federal_transfers_cont = rowSums(select(.,
                                            .dots = all_of(
                                              str_glue("federal_{transfers}_cont")
                                            )),
                                     na.rm = TRUE),
    state_transfers_cont = rowSums(select(.,
                                          .dots = all_of(
                                            str_glue("state_{transfers}_cont")
                                          )),
                                   na.rm = TRUE)
  ) %>% 
    mutate(
      federal_transfers_cont_no_arp = federal_transfers_cont,
      state_transfers_cont_no_arp = state_transfers_cont,
      transfers_cont_no_arp = transfers_cont,
      
      
      federal_transfers_cont = federal_transfers_cont_no_arp + federal_ui_arp_cont + rebate_checks_arp_cont + other_vulnerable_cont + other_direct_aid_cont + aid_to_small_businesses_cont +  health_grants_arp_cont, 
      
      state_transfers_cont = state_transfers_cont_no_arp + state_ui_arp_cont,
      transfers_cont = federal_transfers_cont + state_transfers_cont)
}



coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

source('R/mpc.R')