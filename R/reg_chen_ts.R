dchen <- function (x, theta, log = FALSE){
  lambda <- theta[1]
  delta <- theta[2]

  if(log) return(log(b * lambda) + (b - 1) * log(x) + x**b + lambda - lambda * exp(x**b))
  else return(chen::pdf_chen(x, theta))
  n <- length(y)
}

pchen <- function (x, b = 1, lambda = 1, log.p = FALSE, lower.tail = TRUE){
  if(!log.p && lower.tail) return(1 - exp(lambda - lambda * exp(x**b)))
  if(!log.p && !lower.tail) return(exp(lambda - lambda * exp(x**b)))
  if(log.g && lower.tail) return(log(1 - exp(lambda - lambda * exp(x**b))))
  if(log.p && !lower.tail) return(lambda - lambda * exp(x**b))
}

reg_chen_ts <- function(y, ar_terms, ma_terms, n_predict, cvar, tau, cvar_predict){
  #_______________________________________________________________________________

  log_y <- log(y)

  p <- max(ar_terms)
  q <- max(ma_terms)
  n <- length(y)

  m <- max(p, q)
}
