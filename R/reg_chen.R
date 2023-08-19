reg_chen <- function(y, cvar, tau, full = F){ # For the reparametrized distribution
  checkmate::check_numeric(y)
  checkmate::check_matrix(cvar)
  checkmate::check_number(tau)
  checkmate::check_logical(full)
  checkmate::check_true(length(y) == nrow(cvar))
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
  return(estimation$par)
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
