
# 4.5 MPCs ----------------------------------------------------------------------------------------------

### 4.5.1 Pre-COVID -----------------------------------------------------------------------------------------

mpc_health_outlays = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}

mpc_social_benefits = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}

mpc_noncorp_taxes = function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      lagstart = i-7
      lagend = i-2
      lags = lagstart:lagend
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) 
    }
  }
  j
}

mpc_corporate_taxes = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = -0.4*mean(x[lags]) # distributes out to the MPC applied evenly over 12 quarters
    }
  }
  j
}

mpc_subsidies = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = 0.45*(0.11*x[i]+0.095*x[i-1]+0.09*x[i-2]+0.085*x[i-3]+0.08*x[i-4]+0.08*x[i-5]+0.08*x[i-6]+0.08*x[i-7]+0.075*x[i-8]+0.075*x[i-9]+0.075*x[i-10]+0.075*x[i-11]) 
    }
  }
  j
}



### 4.5.2 Post-COVID ---------------------------------------------------------------------------------------------
#Same as pre-covid
mpc_noncorp_taxes_CRN19 =  function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      lagstart = i-7
      lagend = i-2
      lags = lagstart:lagend
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) 
    }
  }
  j
}


#Same as pre-covid
mpc_health_outlays_CRN19 = function(x){
  0.9*c(SMA(x, n=4))
}

#FIX
mpc_social_benefits_CRN19 = function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      j[i] = 0.86*(0.2879*x[i]+0.2498*x[i-1]+0.19*x[i-2]+0.19*x[i-3] + 0.0253*x[i-4] + 0.0253*x[i-5] + 0.0159*x[i-6] + 0.0159*x[i-7]) # distributes out to 40 percent of the -0.6 MPC applied in first two quarters and the remainder evenly over last 5
    }
  }
  j
}

#Same as pre-covid
mpc_noncorp_taxes_CRN19 =  function(x){
  j = NA
  for(i in 8:length(x)){
    if(is.na(x[i-7])){
      j[i] = NA
    } else{
      lagstart = i-7
      lagend = i-2
      lags = lagstart:lagend
      j[i] = -0.6*(0.2*x[i]+0.2*x[i-1]+(0.6*mean(x[lags]))) 
    }
  }
  j
}





#Same as pre-covid
mpc_corporate_taxes_CRN19 = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = -0.40*mean(x[lags]) 
    }
  }
  j
}