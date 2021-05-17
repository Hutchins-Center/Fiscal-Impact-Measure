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
                       online = FALSE, min_obs = 1)
  
}


#' Marginal Propensity to Consume Benefits in American Rescue Plan for Vulnerable Households
#' Includes UI, Snap, Housing assistance, TANIF, WIC, Cobra Subsidies, etc
#' We assume a cumulative MPC of 0.9. Our timing assumption is that 14% goes out in Q1 and Q2 respectively, 20% in Q3 and Q4 respectively, 9% in Q5, 5% in Q6 and Q7, and 4% in Q8
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_vulnerable_arp <- function(x){
  
  mpc <- 1
  weights <- c(rep(0.14, 2), rep(0.2, 2), 0.09, rep(0.05, 2), 0.04)
  roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE, min_obs = 1)
  
}

mpc_direct_aid_arp <- function(x){
  
  mpc <- 1
  weights <- c(0.18, rep(0.09, 2), rep(0.05, 7), 0.03)
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE, min_obs = 1)
  
}

# 
# mpc_direct_aid_arp <- function(x){
#   
#   mpc <- 1
#   weights <- c(rep(1/8, 8)) 
#   mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
#                        online = FALSE, min_obs = 1)
#   
# }
mpc_small_businesses_arp<- function(x){
  
  mpc <- 1
  weights <- c(rep(0.04, 2), rep(0.017, 10))
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE, min_obs = 1)
  
}

# mpc_small_businesses_arp <- function(x){
#   
#   mpc <- 1
#   weights <- c(rep(1/8, 8))
#   mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
#                        online = FALSE, min_obs = 1)
#   
# }




mpc_ui_arp <- function(x){
  
  mpc <- 1 
  weights <- c(rep(0.253, 2), rep(0.161, 2), 0.075, 0.05, 0.025, 0.022)
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE)
  
}

#' Non-health grants to S&L governments in the American Rescue Plan 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
mpc_arp_non_health_grants<- function(df){
  mpc <- 1
  weights <- c(rep(0.07, 2),
               rep(0.049, 10),
               rep(0.0475, 7),
               0.0375)
  
  df %>% 
    mutate(non_health_grants_post_mpc = mpc * roll::roll_sum(non_health_grants, width = length(weights), weights = rev(weights), online = FALSE, min_obs = 1))
  
}
# 
# mpc_arp_non_health_grants <- function(df){
#   mpc <- 1
#   
#   weights <- c(rep(1 / 8, 8))
#   
#   df %>% 
#     mutate(non_health_grants_post_mpc = mpc * roll::roll_sum(non_health_grants, width = length(weights), weights = rev(weights), online = FALSE, min_obs = 1))
#   
# }



