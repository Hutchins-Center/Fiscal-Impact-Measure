source("R/packages.R")
library("lubridate")
library("janitor")
library("tsibble")
library("dplyr")
# Functions -----------------------------------------------------------------------------------
get_cbo_ui <- function() {
  read_xlsx("data/pandemic-legislation/pandemic-legislation.xlsx",
    sheet = "annual"
  ) %>%
    select(-1) %>%
    pivot_longer(where(is.numeric)) %>%
    pivot_wider(names_from = date, values_from = value) %>%
    clean_names() %>%
    select(-c(economic_impact_payments, coronavirus_relief_fund, na)) %>%
    rename(
      year = name,
      total_ui = total_unemployment_insurance,
      peuc = pandemic_emergency_unemployment_compensation,
      pua = pandemic_unemployment_assistance,
      puc = pandemic_unemployment_compensation_payments
    )
  # annual_to_quarter(year) %>%
  # mutate(total_ui = total_ui + peuc,
  #        peuc = peuc * 2) %>%
  # mutate(fy = fiscal_year(yq)) %>%
  # index_by(fy)
}
legislation_weights <- function(df) {
  df %>%
    mutate(
      peuc = peuc * peuc_weights(),
      pua = pua * pua_weights(),
      puc = puc * puc_weights(),
      other = other * other_weights()
    )
}
decompose_totals <- function(df) {
  df %>%
    mutate(
      ui_legislation = 4 * (peuc + pua + puc + other),
      ui_as = total_ui - ui_legislation
    )
}
ui_allocation <- function(df) {
  df %>%
    rename(
      federal_ui = ui_legislation,
      state_ui = ui_as
    )
}
clean_cbo_ui <- function(df) {
  df %>%
    ungroup() %>%
    index_by(yq) %>%
    mutate(yq = yearquarter(date, fiscal_start = 1))
}

get_monthly_ui <- function() {
  read_xlsx("data/pandemic-legislation/pandemic-legislation.xlsx",
    sheet = "monthly"
  ) %>%
    pivot_longer(where(is.numeric)) %>%
    pivot_wider(names_from = date, values_from = value) %>%
    rename(date = name) %>%
    mutate(date = as_date(as.numeric(date), origin = "1899-12-30")) %>%
    clean_names() %>%
    rename(
      peuc = pandemic_emergency_unemployment_compensation,
      pua = pandemic_unemployment_assistance,
      puc = pandemic_unemployment_compensation_payments,
      wla = wages_lost_assistance_program
    ) %>%
    mutate(
      peuc = 2 * peuc,
      total_ui = unemployment_insurance + wla,
      federal_ui = (peuc + pua + puc + wla),
      state_ui = total_ui - federal_ui
    )
}
monthly_to_quarterly <- function(df) {
  df %>%
    mutate(yq = tsibble::yearquarter(date)) %>%
    as_tsibble(index = date) %>%
    select(date, yq, everything()) %>%
    index_by(yq) %>%
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ mean(.x)
      )
    ) %>%
    distinct(yq, .keep_all = TRUE) %>%
    select(-date)
}
trim_quarterly <- function(df) {
  df %>%
    select(yq, total_ui, federal_ui, state_ui)
}
get_quarterly_ui <- function() {
  read_xlsx("data/pandemic-legislation/pandemic-legislation.xlsx",
    sheet = "quarterly"
  ) %>%
    pivot_longer(where(is.numeric)) %>%
    pivot_wider(names_from = date, values_from = value) %>%
    rename(date = name) %>%
    mutate(yq = yearquarter(date)) %>%
    clean_names() %>%
    rename(
      peuc = pandemic_emergency_unemployment_compensation,
      pua = pandemic_unemployment_assistance,
      puc = pandemic_unemployment_compensation_payments,
      lwa = lost_wages_supplemental_payments,
      total_ui = unemployment_insurance
    ) %>%
    mutate(
      peuc = peuc * 2,
      total_ui = total_ui + lwa
    )
}
peuc_weights <- function() {
  weights_2021 <- c(0.31, 0.42, 0.27, 0)
  return(weights_2021)
}
pua_weights <- function() {
  weights_2021 <- c(0.71, 0.29, 0, 0)
  return(weights_2021)
}
puc_weights <- function() {
  weights_2021 <- c(1, rep(0, 3))
  return(weights_2021)
}
other_weights <- function() {
  weights <- c(rep(0.4, 2), 0.2, 0)
}
fill_quarter <- function(df) {
  length <- df %>%
    count() %>%
    pull()
  missing_months <- 3 - length %% 3
  df %>%
    mutate(date = yearmonth(date)) %>%
    mutate(quarter = yearquarter(date)) %>%
    tsibble::as_tsibble(index = date) %>%
    tsibble::append_row(n = missing_months) %>%
    fill(quarter) %>%
    select(date, quarter, tidyselect::everything())
}



puc_end <- function(df) {
  puc_expiration_date <- "2020-12-31"
  df %>%
    mutate(pandemic_unemployment_compensation_payments = if_else(date > puc_expiration_date, 0,
      pandemic_unemployment_compensation_payments
    ))
}
annualize <- function(x) {
  x * 4
}
deannualize <- function(x) {
  x / 4
}
automatic_stabilizers_weights <- function() {
  weights_2021 <- c(rep(0.26, 2), rep(0.24, 2))
  weights_2022 <- c(0.34, rep(0.22, 3))
  weights_2023 <- c(rep(0.25, 4))

  return(c(weights_2021, weights_2022, weights_2023))
}
automatic_stabilizers_weights <- automatic_stabilizers_weights()
monthly_legislation <- get_monthly_ui()
quarterly_legislation <- get_quarterly_ui()


get_second_draw <- function() {
  read_xlsx("data/pandemic-legislation/pandemic-legislation.xlsx",
    sheet = "stimulus-second-draw-summary"
  ) %>%
    clean_names() %>%
    pivot_wider(names_from = type, values_from = total_b) %>%
    clean_names() %>%
    mutate(
      across(
        where(is.numeric),
        .fns = ~ annualize(.x)
      )
    ) %>%
    mutate(
      date = as_date("2021-03-31"),
      yq = yearquarter(date)
    )
}
get_ui_second_draw <- function(df) {
  df %>%
    rename(ui_second_draw = unemployment_benefits) %>%
    select(yq, ui_second_draw)
}


# Data ----------------------------------------------------------------------------------------
cbo_ui_raw <- get_cbo_ui()
# cbo_ui <-

get_cbo_ui() %>%
  mutate(
    peuc = peuc * 2,
    legislation = peuc + pua + puc + other,
    non_legislation = total_ui - legislation
  ) %>%
  annual_to_quarter(year) %>%
  mutate(fy = fiscal_year(yq)) %>%
  index_by(fy) %>%
  legislation_weights() %>%
  mutate(
    ui_legislation = 4 * (peuc + pua + puc + other),
    ui_as = total_ui - ui_legislation
  ) %>%
  ui_allocation() %>%
  clean_cbo_ui() %>%
  select(date, yq, total_ui, federal_ui, state_ui, peuc, pua, puc, other)

monthly_ui <-
  get_monthly_ui() %>%
  monthly_to_quarterly() %>%
  trim_quarterly()

quarterly_ui <-
  get_quarterly_ui() %>%
  mutate(
    federal_ui = (peuc + pua + puc + lwa),
    state_ui = total_ui - federal_ui
  ) %>%
  select(yq, total_ui, federal_ui, state_ui)

ui_second_draw <-
  get_second_draw() %>%
  get_ui_second_draw()

ui_override <-
  monthly_ui %>%
  full_join(cbo_ui) %>%
  full_join(ui_second_draw) %>%
  mutate(
    ui_second_draw = coalesce(ui_second_draw, 0),
    federal_ui = federal_ui + ui_second_draw,
    state_ui = if_else(yq >= yearquarter("2021 Q2"), 44, state_ui),
    total_ui = federal_ui + state_ui
  ) %>%
  distinct(yq, .keep_all = TRUE) %>%
  mutate(date = lubridate::date(yq) + months(3) - days(1)) %>%
  rename(
    total_ui_override = total_ui,
    federal_ui_override = federal_ui,
    state_ui_override = state_ui
  )


# Export to FIM -------------------------------------------------------------------------------
# projections %>% 
#   fim_create() %>% 
#   full_join(ui_override) %>%
#   mutate(
#     federal_unemployment_insurance = if_else(date > '2020-06-30',
#                                              federal_ui_override,
#                                              federal_unemployment_insurance),
#     state_unemployment_insurance = if_else(date > '2020-06-30',
#                                              state_ui_override,
#                                              state_unemployment_insurance),
#    unemployment_insurance = federal_unemployment_insurance + state_unemployment_insurance 
#   )
  
