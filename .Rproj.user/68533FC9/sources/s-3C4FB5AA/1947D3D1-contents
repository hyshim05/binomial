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

#' @export
print.binvar <- function(x, ...){
  cat('"Binomial variable" \n\n')
  cat("Parameters \n")
  cat("- number of trials:", x$number_trials)
  cat("\n- prob of success:", x$prob_success)
  invisible(x)
}

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

