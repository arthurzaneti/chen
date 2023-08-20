reg_chen <- function(y, cvar, tau, full = F){ # For the reparametrized distribution
  stopifnot(is.numeric(y))
  stopifnot(length(y) == 1)
  stopifnot(is.matrix(cvar) || is.vector(cvar))
  if(is.matrix(cvar)){
    stopifnot(nrow(cvar) == length(y))
  }else{
    stopifnot(length(cvar) == length(y))
  }
  stopifnot(is.numeric(tau))
  stopifnot(length(tau) == 1)
  stopifnot(is.logical(full))
  #_____________________________________________________________________________
  cvar <- cbind(rep(1, length(y)), cvar)
  theta_start = rep(1, ncol(cvar) + 1)
  estimation <- stats::optim(par = theta_start,
                             fn = log_likelihood_rpr_betas,
                             control = list(fnscale = -1),
                             tau = tau,
                             y = y,
                             cvar = cvar,
                             method = "BFGS",
                             hessian = full)
  if(full){
    return(estimation)
  }else{
    return(estimation$par)
  }
}

log_likelihood_rpr_betas <- function(y, theta, tau, cvar){
  lambda <- theta[1]
  betas <- theta[2:length(theta)]
  mus <- exp(cvar %*% betas)
  log_vero <- suppressWarnings(log(log(1 - tau) / (1 - exp(mus^lambda))) +
                                 log(lambda) + (lambda - 1) * log(y) +
                                 (log(1 - tau) / (1 - exp(mus^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
  return(sum(log_vero))
}
