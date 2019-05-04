#' @title Binomial Choose Function
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param k number of trials
#' @param n number of trials
#' @return an integer vector of the number of combinations
#' @export
#' @examples
#' # find 5 choose 3
#' bin_choose(3, 5)
bin_choose <- function(n, k){
  check_success(k, n)
  check_trials(n)

  for(i in 1:length(k)){
    if(k[i] > n){
      stop("k cannot be greater than n")
    }
  }
  combos = factorial(n) / (factorial(k) * factorial(n - k))
  combos
}

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
#' @title Binomial Probability Distribution Function
#' @description creates a list of probabilities of each number of success
#' @param trials number of trials
#' @param prob probability of success
#' @return data frame with class "bindis" and "data.frame"
#' @export
#' @examples
#' # probability of success for each number of success
#' bin_distribution(trials = 5, prob = 0.5)
#'
#' #plotting
#' dis1 <- bin_distribution(5, 0.5)
#' plot(dis1)
bin_distribution <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  success = c(0:trials)
  probability = bin_probability(success, trials, prob)

  distrib <- data.frame(
    success = success,
    probability = probability
  )
  class(distrib) <- c("bindis", "data.frame")
  distrib
}


# plots objects of class "bindis"
#' @export
plot.bindis <- function(x, ...){
  barplot(x$probability, xlab = "successes", ylab = "probability", names.arg = x$success)
}

#' @title Binomial Cumulative Distribution Function
#' @description creates a list of probabilities of each number of success as well as the cumulative probability for each additional success
#' @param trials number of trials
#' @param prob probabiltiy of success
#' @return data frame with class "bincum" and "data.frame"
#' @export
#' @seealso \code{\link{bin_probability}}
#' @seealso \code{\link{bin_choose}}
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#'
#' # plotting
#' dis2 <- bin_cumulative(5, 0.5)
#' plot(dis2)
bin_cumulative <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  success = c(0:trials)
  probability = bin_probability(success, trials, prob)
  cumulative = c(rep(0, length(success)))

  for(i in 2:length(success)){
    cumulative[1] = probability[1]
    cumulative[i] = probability[i] + cumulative[i - 1]
    cumulative
  }

  cumul <- data.frame(
    success = success,
    probability = probability,
    cumulative = cumulative
  )
  class(cumul) <- c("bincum", "data.frame")
  cumul
}

# plots objects of class "bincum"
#' @export
plot.bincum <- function(x, ...) {
  plot(x$success, x$cumulative, type = "o", xlab = "successes", ylab = "probability")
}

#' @title Binomial Random Variable
#' @description finds the variables of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return object of  class "binvar" is a list with named elements: trials and prob
#' @export
#' @examples
#' bin1 <- bin_variable(trials = 10, prob = 0.3)
#' bin1
#'
#' # summary of bin1
#' bin1 <- bin_variable(trials = 10, prob = 0.3)
#' binsum1 <- summary(bin1)
#' binsum1
#'
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)

  number = c(0:trials)
  probability = bin_probability(number, trials, prob)

  variable <- list(
    trials = number,
    probability = probability,
    prob_success = prob,
    number_trials = trials
  )
  class(variable) = "binvar"
  variable
}

# configures the print of any objects of class "binvar"
#' @export
print.binvar <- function(x, ...){
  cat('"Binomial variable" \n\n')
  cat("Parameters \n")
  cat("- number of trials:", x$number_trials)
  cat("\n- prob of success:", x$prob_success)
  invisible(x)
}

# summary functions for objects of class "binvar"
#' @export
summary.binvar <- function(x, ...){
  summary <- list(
    trials = x$number_trials,
    prob = x$prob_success,
    mean = aux_mean(x$number_trials, x$prob_success),
    variance = aux_variance(x$number_trials, x$prob_success),
    mode = aux_mode(x$number_trials, x$prob_success),
    skewness = aux_skewness(x$number_trials, x$prob_success),
    kurtosis = aux_kurtosis(x$number_trials, x$prob_success)
  )
  class(summary) = "summary.binvar"
  summary

}

# configures the print of summary() of any objects of class "binvar"
#' @export
print.summary.binvar <- function(x, ...){
  cat('"Summary Binomial" \n\n')
  cat("Parameters \n")
  cat("- number of trials:", x$number_trials)
  cat("\n- prob of success:", x$prob_success)
  cat("\n\nMeasures")
  cat("\n- mean:", x$mean)
  cat("\n- variance:", x$variance)
  cat("\n- mode:", x$mode)
  cat("\n- skewness:", x$skewness)
  cat("\n- kurtosis:", x$kurtosis)
}

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
