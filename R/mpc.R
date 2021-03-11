#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_rebate_checks <- function(x){

  
  mpc <- 0.67
  weights <- c(0.238, 0.145, 0.11, rep(0.079, 3), rep(0.084, 2), rep(0.039, 2), 0.023)
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE)
  
}

#' Marginal propensity to consume for Subsidies 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
mpc_unemployment_insurance <- function(x){
  
  mpc <- 0.9 
  weights <- c(rep(0.253, 2), rep(0.161, 2), 0.075, 0.05, 0.025, 0.022)
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE)
    
}

