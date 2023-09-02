dchen <- function (x, theta, log = FALSE){

  if(log) return(chen::ll_chen(x, theta))
  else return(chen::pdf_chen(x, theta))
  n <- length(y)
}

pchen <- function (x, b = 1, lambda = 1, log.p = FALSE, lower.tail = TRUE){
  if(!log.p && lower.tail) return(1 - exp(lambda - lambda * exp(x**b)))
  if(!log.p && !lower.tail) return(exp(lambda - lambda * exp(x**b)))
  if(log.g && lower.tail) return(log(1 - exp(lambda - lambda * exp(x**b))))
  if(log.p && !lower.tail) return(lambda - lambda * exp(x**b))
}

reg_chen_ts <- function(y, ar = NULL, ma = NULL, n_predict = NULL, cvar = NULL, tau = 0.5, cvar_predict = NULL){
  #_______________________________________________________________________________

  log_y <- log(y)

  isma <- checkmate::check_vector(ar, null.ok = T)
  isar <- checkmate::check_vector(ma, null.ok = T)
  if(isar) max_ar <- max(ar)
  if(isma) max_ma <- max(ar)

  n <- length(y)
  if(is.null(n_predict)) n_total <- n
  else n_total <- n + n_predict

  max_arma <- max(max_ma, max_ar, na.rm = T)
  n_fit <- n - max_arma
  print(n)
  print(n_total)

  y_prev <- c(rep(NA, n_total))
  if(isar){

    n_ar <- length(ar)
    mat <-matrix(rep(NA, n_fit * n_ar), ncol = n_ar)

    indices <- 1:n_fit
    mat[indices] <- log_y[indices + max_arma - ar]
    print(mat)
  }
}
