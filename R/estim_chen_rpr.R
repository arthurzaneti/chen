#' @title Parameter estimation for the reparameterized Chen distribution
#' @description
#' The Maximum likelihood estimate for the pair of parameters is numerically approximated with optim from the stats package. The estim_chen function is effectivelly a wrapper for optim, but it defines and uses the Chen log-likelihood function as well.
#'
#'
#' @param y Values sampled from a reparameterized Chen distribution.
#'
#' @return The pair of estimated parameters. If full == TRUE than will return the same list as the optim function.
#' If CI = TRUE will return the confidence interval.
#' @export
#'
#' @examples
#' estim_chen(rchen(10, c(1,1)))
#'
estim_chen_rpr <- function(y, method = "BFGS", tau = 0.5, full = F, ci_alpha = NULL){
  checkmate::check_numeric(y)
  checkmate::check_choice(method, c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"))
  checkmate::check_number(tau)
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
  if(!is.null(ci_alpha)){
    par <- estim$par
    inf <- solve(-estim$hessian)
    a <- qnorm(1 - ci_alpha/2) * sqrt(diag(inf))
    # The name a here is arbitrary;
    # It is used just to store a part of the confidence interval calculation
    lower_bound <- par - a
    upper_bound <- par + a
    ci <- rbind(lower_bound, upper_bound)
    if(full){
      estim$confidence_intervals <- ci
      return(estim)
    }else{
      return(rbind(par, ci))
    }
  }else{
    if(full){
      return(estim)
    }else{
      return(par)
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
