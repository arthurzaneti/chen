estim_chen_rpr <- function(y, method = "BFGS", tau = 0.5){

  func <- log_likelihood
  suppressWarnings(estim <- stats::optim(par= c(1, 2),
                                         fn = log_likelihood,
                                         y = y,
                                         tau = tau,
                                         method = method,
                                         hessian = F,
                                         control = list(fnscale = -1)))
  return(estim)
}

log_likelihood_rpr <- function(y, theta, tau){

  lambda <- theta[1]
  mu <- theta[2]
  n <- length(y)
  ll <- n*log(delta)+n*log(lambda) + (lambda-1)*log(y) +
    delta*(1-exp(y^lambda))+ y^lambda
  return(sum(ll))
}
