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


