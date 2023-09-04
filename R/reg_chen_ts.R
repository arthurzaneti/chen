#' Title
#'
#' @param y
#' @param ar
#' @param ma
#' @param cvar
#' @param tau
#'
#' @return
#' @export
#'
#' @examples

reg_chen_ts <- function(y, ar = NULL, ma = NULL, cvar = NULL, tau = 0.5){
  if(!any(is.null(cvar))) cvar <- as.matrix(cvar)
  #__________________________________ORGANIZING____________________________________

  # log_y <- log(y)

  isar <- !chen::is_null(ar)
  isma <- !chen::is_null(ma)
  isreg <- !chen::is_null(cvar)

  case <- chen::arma_case(isar, isma, isreg)

  return(switch(case,
                "ARMA" = ARMA(y, ar, ma, tau), #max_ar, max_ma, ar, ma, max_arma, tau, mqo,
                "AR" = AR(y, ar, tau),
                "MA" = MA(),
                "REG_ARMA" = REG_ARMA(),
                "REG_AR" = REG_AR(),
                "REG_MA" = REG_MA()))

}
#____________________________________CASES________________________________________
ARMA <- function(y, ar, ma, tau){
  log_y <- log(y)
  max_arma <- max(max_ma <- max(ma), max_ar <- max(ar))
  n <- length(y)
  n_fit <- n - max_arma
  n_ar <- length(ar)
  n_ma <- length(ma)
  y_cut<- y[(max_arma + 1): n]
  log_y_cut <- log_y[(max_arma + 1): n]

  mat <- matrix(rep(NA, n_fit * n_ar), ncol = n_ar)
  indices <- 1:n_fit
  for(i in 1:length(ar)){
    mat[indices, i] <- log_y[indices + max_arma - ar[i]]
  }

  mat <- cbind(rep(1, n_fit), mat)
  opt <- stats::lm.fit(mat, log_y_cut)
  mqo <- stats::coef(opt)

  #___________________________________
  names_phi <- c(paste("phi", ar, sep = ""))
  names_rho <- c(paste("rho", ma, sep = ""))
  names_par <- c("intercept", names_phi, names_rho, "lambda")

  opt <- stats::optim(c(mqo, rep(0, n_ma), 0.7),
                      chen::ll_ARMA,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ar = n_ar, n_ma = n_ma, ar = ar,
                      ma = ma, max_arma = max_arma, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <-opt$par
  names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- phi <- coef[2:(max_ar + 1)]
  model$rho <- rho <- coef[(max_ar + 2): (max_ar + max_ma + 1)]
  model$lambda <- lambda <- coef[(max_ar + max_ma + 2)]
  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_arma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_arma + 1):n){
    etahat[i] <- beta0 + (phi %*% log_y[i - ar]) + (rho %*% errorhat[i - ma])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_arma + 1):n])
  model$fitted <- ts(c(rep(NA, max_arma), muhat), start = start(y), frequency = frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "ARMA"
  class(model) <- "CHARMA_reg"
  return(model)
}
#_______________________________________________________________________________________

AR <- function(y, ar, tau){
  log_y <- log(y)
  max_ar <- max(ar)
  n <- length(y)
  n_fit <- n - max_ar
  n_ar <- length(ar)
  y_cut <- y[(max_ar + 1): n]
  log_y_cut <- log_y[(max_ar + 1): n]

  mat <- matrix(rep(NA, n_fit * n_ar), ncol = n_ar)
  indices <- 1:n_fit
  for(i in 1:length(ar)){
    mat[indices, i] <- log_y[indices + max_ar - ar[i]]
  }

  mat <- cbind(rep(1, n_fit), mat)
  opt <- stats::lm.fit(mat, log_y_cut)
  mqo <- stats::coef(opt)

  #___________________________________
  names_phi <- c(paste("phi", ar, sep = ""))
  names_par <- c("intercept", names_phi, "lambda")

  opt <- stats::optim(c(mqo, 0.7),
                      chen::ll_AR,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ar = n_ar, ar = ar,
                      max_ar = max_ar, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <-opt$par
  names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- phi <- coef[2:(max_ar + 1)]
  model$lambda <- lambda <- coef[(max_ar + 2)]
  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_ar), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_ar + 1):n){
    etahat[i] <- beta0 + (phi %*% log_y[i - ar])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_ar + 1):n])
  model$fitted <- ts(c(rep(NA, max_ar), muhat), start = start(y), frequency = frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "AR"
  class(model) <- "CHARMA_reg"
  return(model)

}
MA <- function(){

}
REG_ARMA <- function(){

}
REG_AR <- function(){

}
REG_MA <- function(){

}







