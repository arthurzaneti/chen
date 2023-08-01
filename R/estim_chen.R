#' @title Parameter estimation for the Chen distribution
#' @description
#' The Maximum likelihood estimate for the pair of parameters is numerically approximated with optim from the stats package. The estim_chen function is effectivelly a wrapper for optim, but it defines and uses the Chen log-likelihood function as well.
#'
#'
#' @param y Values sampled from a Chen distribution.
#'
#' @return The best set of parameters found
#' @export
#'
#' @examples
#' estim_chen(rchen(10, c(1,1)))
#'
estim_chen <- function(y, method = "BFGS"){

  func <- log_likelihood
  suppressWarnings(estim <- stats::optim(par = c(1, 0.5),
                        fn = log_likelihood,
                        y = y,
                        method = method,
                        hessian = F,
                        control = list(fnscale = -1)))
  return(estim)
}

log_likelihood <- function(y, theta){

  lambda <- theta[1]
  delta <- theta[2]
  n <- length(y)
  ll <- n * (2 * log(delta) + log(lambda)) +
    sum((lambda-1) * log(y) - delta * exp(y ^ lambda) + y ^lambda)
  return(sum(ll))
}


