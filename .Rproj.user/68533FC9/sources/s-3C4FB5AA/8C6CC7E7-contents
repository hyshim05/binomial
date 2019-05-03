# Auxiliary Functions

# private auxiliary function to find mean
aux_mean <- function(trials, prob){
  mean = trials*prob
  return(mean)
}

# private auxiliary function to find variance
aux_variance <- function(trials, prob){
  variance = trials * prob * (1 - prob)
  return(variance)
}

# private auxiliary function to find mode
aux_mode <- function(trials, prob){
  if(trials %%2 == 0 & trials >= 1 & prob >= 0 & prob <= 1){
    mode = floor((trials * prob) + prob)
    return(mode)
  }
  else if(trials %%2 != 0 & trials >= 1 & prob >= 0 & prob <= 1){
    m = floor((trials * prob) + prob)
    mode2 = c(m, m - 1)
    return(mode2)
  }
}

# private auxiliary function to find skewness
aux_skewness <- function(trials, prob){
  skewness <- (1 - 2*prob) / sqrt(trials*prob*(1 - prob))
  return(skewness)
}

# private auxiliary function to find kurtosis
aux_kurtosis <- function(trials, prob){
  kurtosis = (1 - 6*prob * (1 - prob)) / (trials * prob * (1 - prob))
  return(kurtosis)
}


