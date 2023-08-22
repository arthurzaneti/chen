#' @title Parameter estimation for the reparameterized Chen distribution
#' @description
#' The Maximum likelihood estimate for the pair of parameters is numerically
#' approximated with optim from the stats package. The estim_chen function is
#' effectivelly a wrapper for optim, but it defines and uses the Chen
#' log-likelihood function as well.
#'
#'
#' @param y Values sampled from a reparameterized Chen distribution.
#' @param method The optimisation method used by optim, default is BFGS
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
#' @importFrom stats median optim qnorm
#' @export
#'
#' @examples
#' estim_chen(rchen(10, c(1,1)))
#'
estim_chen <- function(y, method = "BFGS", full = F, clvl = NULL){
  checkmate::check_numeric(y)
  checkmate::check_choice(method, c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"))
  checkmate::check_logical(full)
  if(!is.null(clvl)) checkmate::check_number(clvl)
  #__________________________________end_checks_________________________________

  lambda_start <- 1
  delta_start <- 2
  suppressWarnings(estim <- stats::optim(par= c(lambda_start, delta_start),
                                         fn = log_likelihood,
                                         y = y,
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


log_likelihood <- function(y, theta){

  lambda <- theta[1]
  delta <- theta[2]
  n <- length(y)
  ll <- n * log(delta) + n * log(lambda) +
    sum((lambda - 1) * log(y) + delta * (1 - exp(y^lambda)) + y^lambda)
  return(ll)
}


