#' @title Parameter estimation for the reparameterized Chen distribution
#' @description
#' The Maximum likelihood estimate for the pair of parameters is numerically
#' approximated with optim from the stats package. The estim_chen_rpr function is
#' effectivelly a wrapper for optim, but it defines and uses the Chen
#' log-likelihood function as well.
#'
#'
#' @param y Values sampled from a reparameterized Chen distribution.
#' @param method The optimisation method useed by optim, default is BFGS
#' @param tau The quantile for the reparameterized distribution
#' @param full If the return should be the entire list by optim or just the parameter
#' estimations
#' @param clvl The confidence level for calculating the confidence intervals. If NULL no confidence
#' intervals will be calculated. Default in NULL.
#' @return If clvl is set to NULL (default) and full is set to TRUE(default)
#'  than the return is lenght 2 numeric vector with the parameter estimations. If
#'  clvl is a number, than the return is a 3x2 matrix with the parameter estimations
#'  and their confidence intervals. If full is set to TRUE than the return is a
#'  list, having by default 6 elements, if clvl is a number than the list will
#'  have 7 elements with the last one being the matrice of confidence intervals.
#' @importFrom stats median, optim, qnorm
#' @export
#'
#' @examples
#' estim_chen_rpr(rchen(100, c(0.7,7)), tau = 0.5)
#'
estim_chen_rpr <- function(y, method = "BFGS", tau = 0.5, full = F, clvl = NULL){
  checkmate::check_numeric(y)
  checkmate::check_choice(method, c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"))
  checkmate::check_number(tau, lower = 0, upper = 1)
  checkmate::check_logical(full)
  if(!is.null(clvl)) checkmate::check_number(clvl)
  #__________________________________end_checks_________________________________
  lambda_start <- 1
  mu_start <- stats::median(y)
  suppressWarnings(estim <- stats::optim(par= c(lambda_start, mu_start),
                                         fn = log_likelihood_rpr,
                                         y = y,
                                         tau = tau,
                                         method = method,
                                         hessian = T,
                                         control = list(fnscale = -1)))
  param <- estim$par
  if(!is.null(clvl)){
    inf <- solve(-estim$hessian)
    a <- stats::qnorm(1 - (1 - clvl)/2) * sqrt(diag(inf))
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
