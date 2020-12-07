# define some helper functions function to pull haver data
#' Title
#'
#' @param series variables to search for in Haver
#' @param database Haver name
#' @param start.date 
#' @param frequency can be annual or quarterly. Default is quarterly
#'
#' @return data frame
pull_data <- function(series, database, start.date, frequency = "quarterly") {
  q <- haver.data(series, database, eop.dates = T, start = as.Date(start.date, 
                                                                   f = "%m-%d-%Y"))
  q <- data.frame(date = as.Date(rownames(q)), q)
  
  for (j in 2:ncol(q)) {
    for (k in 4:nrow(q)) {
      if (is.na(q[k, j])) {
        q[k, j] = mean(q[c(k - 1, k - 2, k - 3), j])  # if the data is missing on unreported, use the 3-qtr moving average
        
      }
    }
  }
  q
}

# function to calculate quarterly annualized growth rates
#' Title
#'
#' @param x is a time series object
#'
#' @return annualized quarterly growth rate
#' @export
#'
#' @examples
q_a = function(x) {
  j = c()
  for (i in 2:length(x)) {
    j[i] = (((x[i]/x[i - 1])^4) - 1) * 100
  }
  j[1] = 0
  j
}

# function to calculate quarter-over-quarter growth rate
q_g = function(x) {
  j = c()
  for (i in 2:length(x)) {
    j[i] = (((x[i]/x[i - 1])) - 1)
  }
  j[1] = j[2]
  j
}

#' Title: Growth rate
#' Description: Replaces q_g and q_a function
#' @param x A numeric vector of values
#' @param period "qoq" (quarter-over-quarter) or "annualized" growth rate
#' 
#' @return A numeric vector with the growth rate of the input
#' @example growthRate(econ$gf, period =  "qoq")
growthRate <- function(x, period) {
  rate <- c()
  if (period == "annualized") {
    rate <- (((x/lag(x))^4) - 1) * 100
    rate[1] = 0
  } 
  else if (period == "qoq") {
    rate <- (x/lag(x)) - 1
    rate[1] = rate[2]
  }
  return(rate)
}

# qoqGrowth <- function(.data, expr){ .data <- .data %>%
# mutate( '{{expr}}_g' := ( {{expr}} / lag( {{expr}} ) ) - 1
# ) #na.locf(.data[,paste0((as_label(enquo(expr))), '_g')])
# .data } annualizedGrowth <- function(data, expr){ data %>%
# mutate( '{{expr}}_g' := ((( {{expr}} / lag( {{expr}} )
# )^4)-1)*100 ) }


#' Title
#'
#' @param .data data frame with components needed to calculate contributions
#' @param var federal_nom, state_local_nom, federal_cgrants, federal_igrants
#'
#' @return contribution of var to real gdp growth
#' @export
#'
#' @examples
contribution <- function(.data, var){
  var <- ensym(var) # quote expression
  var_string <- rlang::as_string(enexpr(var)) # convert quoted expression to string
  deflator_string <- paste0(var_string, "_pi") # create string with the name of relevant deflator
  deflator <- rlang::sym(deflator_string) # convert deflator string to symbol
  
  ## Calculate contribution
  .data %>%
    mutate(
      "{{ var }}_cont" := 400 * ({{ var }} - (1  + !!(deflator) + gdppoth) * lag({{ var }}) ) / lag(gdp)
    ) %>%
    select(date, !!paste0(var_string, "_cont"))
}

#' Title
#'
#' @param .data data frame with components needed to calculate contributions
#' @param var federal_nom, state_local_nom, federal_cgrants, federal_igrants
#'
#' @return contribution of var to real gdp growth
#' @export
#'
#' @examples
contribution_nipas <- function(.data, var){
  var <- ensym(var) # quote expression
  var_string <- rlang::as_string(enexpr(var)) # convert quoted expression to string
  deflator_string <- paste0(var_string, "_pi") # create string with the name of relevant deflator
  deflator <- rlang::sym(deflator_string) # convert deflator string to symbol
  
  ## Calculate contribution
  .data %>%
    mutate(
      "{{ var }}_cont" := 400 * ({{ var }} - (1  + !!(deflator)) * lag({{ var }}) ) / lag(gdp)
    ) %>%
    select(date, !!paste0(var_string, "_cont"))
}


output_xlsx <- function(data, names){ 
  folder_path <- paste0("results/", thismonth, '/')
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}

source('src/mpcs.R')
