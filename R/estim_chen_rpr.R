#' @title Parameter estimation for the reparameterized Chen distribution
#' @description
#' The Maximum likelihood estimate for the pair of parameters is numerically approximated with optim from the stats package. The estim_chen function is effectivelly a wrapper for optim, but it defines and uses the Chen log-likelihood function as well.
#'
#'
#' @param y Values sampled from a reparameterized Chen distribution.
#' @param method The optimisation method useed by optim, default is BFGS
#' @param tau The quantile for the reparameterized distribution
#' @param full If the return should be the entire list by optim or just the parameter
#' estimations
#' @param ci_alpha The alpha for calculating the confidence intervals. If null no confidence
#' intervals will be calculated. Default in NULL
#'
#' @return The pair of estimated parameters. If full == TRUE than will return the same list as the optim function.
#' If CI = TRUE will return the confidence interval.
#' @export
#'
#' @examples
#' estim_chen_rpr(rchen(100, c(0.7,7)), tau = 0.5)
#'
estim_chen_rpr <- function(y, method = "BFGS", tau = 0.5, full = F, ci_alpha = NULL){
  checkmate::check_numeric(y)
  checkmate::check_choice(method, c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"))
  checkmate::check_number(tau, lower = 0, upper = 1)
  checkmate::check_logical(full)
  if(!is.null(ci_alpha)) checkmate::check_number(ci_alpha)
  #__________________________________end_checks_________________________________

  suppressWarnings(estim <- stats::optim(par= c(1, 2),
                                         fn = log_likelihood_rpr,
                                         y = y,
                                         tau = tau,
                                         method = method,
                                         hessian = T,
                                         control = list(fnscale = -1)))
  param <- estim$par
  if(!is.null(ci_alpha)){
    inf <- solve(-estim$hessian)
    a <- stats::qnorm(1 - ci_alpha/2) * sqrt(diag(inf))
    # The name a here is arbitrary;
    # It is used just to store a part of the confidence interval calculation
    lower_bound <- param - a
    upper_bound <- param + a
    ci <- rbind(lower_bound, upper_bound)
    if(full){
      estim$confidence_intervals <- ci
      return(estim)
    }else{
      return(rbind(param, ci))
    }
  }else{
    if(full){
      return(estim)
    }else{
      return(param)
    }
  }
}

log_likelihood_rpr <- function(y, theta, tau){
  lambda <- theta[1]
  mu <- theta[2]
  n <- length(y)
  ll <- suppressWarnings(sum((log(log(1 - tau) / (1 - exp(mu^lambda))) +
                           log(lambda) + (lambda - 1) * log(y) +
                           (log(1 - tau) / (1 - exp(mu^lambda))) * (1 - exp(y^lambda)) + (y^lambda))))
  return(ll)
}
