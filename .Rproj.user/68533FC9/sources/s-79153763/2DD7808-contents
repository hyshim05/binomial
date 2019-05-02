#' @title Binomial Mean Function
#' @description finds the binomial mean
#' @param trials number of trials
#' @param prob probability of success
#' @return expected mean
#' @export
bin_mean <- function(trials, prob){
  mean = aux_mean(trials, prob)
  return(mean)
}

# private auxiliary function to find mean
aux_mean <- function(trials, prob){
  mean = trials*prob
  return(mean)
}

#' @title Binomial Variance Function
#' @description finds the binomial variance
#' @param trials number of trials
#' @param prob probability of success
#' @return variance
#' @export
bin_variance <- function(trials, prob){
  variance = aux_variance(trials, prob)
  return(variance)
}

# private auxiliary function to find variance
aux_variance <- function(trials, prob){
  if(trials %%1 == 0 & trials >= 1 & prob >= 0 & prob <= 1){
    variance = trials * prob * (1 - prob)
    return(variance)
  }
  else{
    stop("Invalid trials or prob values, trials must be a non-negative integer and prob must be between 1 and 0")
  }
}

#' @title Binomial Mode Function
#' @description finds the binomial mode
#' @param trials number of trials
#' @param prob probability of success
#' @return found mode
#' @export
bin_mode <- function(trials, prob){
  mode =aux_mode(trials, prob)
  return(mode)
}

# auxiliary mode
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
  else{
    stop("Invalid trials or prob values, trials must be a non-negative integer and prob must be between 1 and 0")
  }
}

#' @title Binomial Skewness Function
#' @description finds the asymmetry of the probability distribution of a random variable about its mean
#' @param trials number of trials
#' @param prob probability of success
#' @return found skewness
#' @export
bin_skewness <- function(trials, prob){
  skewness = aux_skewness(trials, prob)
  return(skewness)
}

# private auxiliary function to find skewness
aux_skewness <- function(trials, prob){
  skewness <- (1 - 2*prob) / sqrt(trials*prob*(1 - prob))
  return(skewness)
}

#' @title Binomial Kurtosis Function
#' @description finds the the Kurtosis of a  probability distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return found Kurtosis
#' @export
bin_kurtosis <- function(trials, prob){
  kurtosis = aux_kurtosis(trials, prob)
  return(kurtosis)
}

# private auxiliary function to find kurtosis
aux_kurtosis <- function(trials, prob){
  kurtosis = (1 - 6*prob * (1 - prob)) / (trials * prob * (1 - prob))
  return(kurtosis)
}

