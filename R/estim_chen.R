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
estim_chen <- function(y, method = "BFGS", full = F, ci_alpha = NULL){
  checkmate::check_numeric(y)
  checkmate::check_choice(method, c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"))
  checkmate::check_logical(full)
  if(!is.null(ci_alpha)) checkmate::check_number(ci_alpha)
  #__________________________________end_checks_________________________________

  suppressWarnings(estim <- stats::optim(par= c(1, 2),
                                         fn = log_likelihood,
                                         y = y,
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


log_likelihood <- function(y, theta){

  lambda <- theta[1]
  delta <- theta[2]
  n <- length(y)
  ll <- sum(n * (2 * log(delta) + log(lambda)) +
    sum((lambda-1) * log(y) - delta * exp(y ^ lambda) + y ^lambda))
  return(ll)
}


