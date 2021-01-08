# 4.5 MPCs ----------------------------------------------------------------------------------------------
### 4.5.1 Pre-COVID -----------------------------------------------------------------------------------------
mpc_health_outlays = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}

mpc_social_benefits = function(x){
  0.9 * rollapply(x, width = 4, mean, partial = TRUE, fill = NA, align =  'right')
}
# Same as mpc_corporate_taxes
mpc_corporate_taxes <- function(x){
  mpc <- -0.4
  mpc * rollapply(x, width = 12, mean, fill = NA, align =  'right')
}

mpc_corporate_taxes_CRN19 <- mpc_corporate_taxes

# Same as mpc noncorp taxes
mpc_noncorp_taxes <- function(x){
  mpc <- -0.6
  weights <- c(rep(0.1, 6), rep(0.2, 2))
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}

mpc_noncorp_taxes_CRN19 <- mpc_noncorp_taxes

mpc_subsidies <- function(x) {
  mpc <- 0.45
  weights <- c(rep(0.075,4), rep(0.08,4), 0.085, 0.09, 0.095, 0.11)
  mpc * roll::roll_sum(x, width = length(weights), 
                       weights = weights, online = FALSE)
}


mpc_ppp_round2 <- function(x){
  mpc <- 0.525
  weights <- c(rep(0.0625, 4), rep(0.0750, 4), 0.0875, 0.1, 0.1125, 0.15)
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}

mpc_unemployment_insurance <- function(x){
  mpc <- 0.9
  weights <- c(rep(0.05, 2), rep(0.1, 2), rep(0.35, 2))
  mpc * roll::roll_sum(x, width = length(weights), 
                       weights = weights, online = FALSE)
}
mpc_ui_CRN19 = function(x){
  j = NA
  for(i in 6:length(x)){
    if(is.na(x[i-5])){
      j[i] = NA
    } else{
      j[i] = 0.9*(0.35*x[i]+ 0.35*x[i-1]+0.1*x[i-2]+0.1*x[i-3] + 0.05*x[i-4] + 0.05*x[i-5]) # distributes out to 40 percent of the -0.6 MPC applied in first two quarters and the remainder evenly over last 5
    }
  }
  j
}

### 4.5.2 Post-COVID ---------------------------------------------------------------------------------------------


#Same as pre-covid
mpc_health_outlays_CRN19 = function(x){
  0.9*c(SMA(x, n=4))
}


mpc_social_benefits_CRN19 <- function(x){
  mpc <- 0.86
  weights <- c(rep(0.0159, 2), rep(0.0253, 2), rep(0.19, 2), 0.2498, 0.2879)
  mpc * roll::roll_sum(x, width = length(weights),
                 weights = weights, online = FALSE)
}

# Regular	0.9	62%	0.25	0.25	0.25	0.25				
# Total UI	0.9	19%	0.35	0.35	0.1	0.1	0.05	0.05	0	0
# Rebate Checks	0.7	19%	0.35	0.15	0.08	0.08	0.08	0.08	0.08	0.08
# 
mpc_social_benefits_ex_ui_rebate <- 
  function(x){
    0.9 * rollapply(x, width = 4, mean,
                    partial = TRUE, fill = NA, 
                    align =  'right')
  }

mpc_rebate_CRN19 = function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      j[i] = 0.7*(0.35*x[i]+0.15*x[i-1]+0.08*x[i-2]+0.08*x[i-3] + 0.08*x[i-4] + 0.08*x[i-5] + 0.08*x[i-6] + 0.08*x[i-7]) # distributes out to 40 percent of the -0.6 MPC applied in first two quarters and the remainder evenly over last 5
    }
  }
  j
}
mpc_rebate_checks <- function(x){
  mpc <- 0.7
  weights <- c(rep(0.08, 6), 0.15, 0.35)
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}
mpc_rebate_round2 <- function(x){
  mpc <- 0.7
  weights <- c(rep(0.08, 6), 0.15, 0.35)
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}






