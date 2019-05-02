#' @title Binomial Probability Function
#' @description uses the bin_choose() function to calculate that probability of getting a number of successes in a number of trials
#' @param success number of success
#' @param trials number of trials
#' @param prob probability of success
#' @return the probability or list of probabilities
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials with prob of success = 0.5
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' probabilities of getting 2 or less successes in 5 trials with prob of success = 0.5
#' bin_probability(0:2, 5, 0.5)
#'
#' probability of getting 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(55, 100, 0.45)
bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  probability = bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success)
  probability
}

# private checker function to ensure that probability, p, is valid i.e. between 1 and 0
check_prob <- function(p){
  if(p < 0 | p > 1){
    stop("Invalid probability value, p has to be a number between 0 and 1")
  }
  else{
    return(TRUE)
  }
}

# private checker function to ensure that the value of trials is a non-negative integer
check_trials <- function(trials){
  if(trials %% 1 != 0 | trials < 0){
    stop("Invalid trials value, trials must be non-negative interger value")
  }
  else{
    return(TRUE)
  }
}

# private checker function to ensure that successes are non-negative integer value that is less than or equal to trials
check_success <- function(success, trials){
  for(i in 1:length(success)){
    if(success[i] %% 1 != 0 | success[i] > trials){
      stop("Invalid success value, success cannot be greater than trials")
    }
  }
  return(TRUE)
}

