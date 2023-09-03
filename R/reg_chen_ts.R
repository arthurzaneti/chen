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

reg_chen_ts <- function(y, ar = NULL, ma = NULL, cvar = NULL, tau = 0.5){
  if(!any(is.null(cvar))) cvar <- as.matrix(cvar)
  #__________________________________ORGANIZING____________________________________

  log_y <- log(y)

  isar <- !chen::is_null(ar)
  isma <- !chen::is_null(ma)
  isreg <- !chen::is_null(cvar)

  case <- chen::arma_case(isar, isma, isreg)

  if(isma && isar) {
    max_arma <- max(max_ma <- max(ma), max_ar <- max(ar))
  } else if(isar) {
    max_arma <- max_ar <- max(ar)
  } else if(isma) {
    max_arma <- max_ma <- max(ma)
    print(max_arma)
  }

  n <- length(y)
  n_fit <- n - max_arma

  y_cut<- y[(max_arma + 1): n]
  log_y_cut <- log_y[(max_arma + 1): n]

  y_prev <- c(rep(NA, n))

  if(isar){
    n_ar <- length(ar)
    mat <-matrix(rep(NA, n_fit * n_ar), ncol = n_ar)
    indices <- 1:n_fit
    for(i in ar){
      mat[indices, i] <- log_y[indices + max_arma - i]
    }
    mat <- cbind(rep(1, n_fit), mat)
  }else{
    mat <- as.matrix(rep(1, n_fit))
  }

  if(isreg){
    # Starting parameters
    opt <- stats::lm.fit(mat, log_y_cut)
    start <- stats::coef(opt)
    n_par <- length(start)
    lambda <- 0.7

  } else {

    x <- cbind(mat, cvar[(max_arma + 1) : n, ])
    opt <- stats::lm.fit(mat, log_y_cut)
    start <- stats::coef(opt)
    n_par <- length(start)
    lambda <- 0.7

    dlog <- 1/exp(stats::fitted(opt))
    err <- stats::residuals(opt)
  }
  lambda <- 0.7
  #_______________________________________________________________________________
  switch(case,
         "ARMA" = ARMA(),
         "AR" = AR(),
         "MA" = MA(),
         "REG_ARMA" = REG_ARMA(),
         "REG_AR" = REG_AR(),
         "REG_MA" = REG_MA())
}

ARMA <- function(lambda, start, y, n, n_fit, y_cut, log_y, log_y_cut, max_arma, max_ar, max_ma){

}
AR <- function(){

}
MA <- function(){

}
REG_ARMA <- function(){

}
REG_AR <- function(){

}
REG_MA <- function(){

}








