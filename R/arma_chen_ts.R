#' CHARMA fitting without regressors
#'
#' Used for fitting the Chen auto-regressive moving averages model with regressors. The model is given by
#' \deqn{\beta_{0} +
#'\sum_{j\in ar}\phi_j \log(y_{t-j})
#' +\sum_{j\in ma}\theta_jr_{t-j}}
#' Where \itemize{
#' \item{\eqn{y} are the variables}
#' \item{\eqn{\beta_{0}} is the intercept}
#' \item{\eqn{\boldsymbol{x}_t} are the covariables}
#' \item{\eqn{\boldsymbol{\beta}} are the regression coefficients}
#' \item{\eqn{ar} are the indices for the auto-regression}
#' \item{\eqn{ma} are the indices for the moving-averages}
#' \item{\eqn{\phi} are the auto-regression coefficients}
#' \item{\eqn{\theta} are the moving-averages coefficients}
#' \item{\eqn{r} are the errors}}
#'
#' @param y The vector of variables to fit
#' @param ar Specified in description.
#' @param ma Specified in description.
#' @param tau The quantile
#'
#' @return
#' @export
#'
#' @examples

arma_chen_ts <- function(y, ar = NULL, ma = NULL, tau = 0.5){

  #__________________________________ORGANIZING____________________________________

  # log_y <- log(y)

  isar <- !chen::is_null(ar)
  isma <- !chen::is_null(ma)

  case <- chen::reg_arma_case(isar, isma, F)

  return(switch(case,
                "ARMA" = ARMA(y, ar, ma, tau),
                "AR" = AR(y, ar, tau),
                "MA" = MA(y, ma, tau)))
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
  names_phi <- paste("phi", ar, sep = "")
  names_theta <- paste("theta", ma, sep = "")
  names_par <- c("intercept", names_phi, names_theta, "lambda")

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
  model$theta <- theta <- coef[(max_ar + 2): (max_ar + max_ma + 1)]
  model$lambda <- lambda <- coef[(max_ar + max_ma + 2)]
  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_arma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_arma + 1):n){
    etahat[i] <- beta0 + (phi %*% log_y[i - ar]) + (theta %*% errorhat[i - ma])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_arma + 1):n])
  model$fitted <- ts(c(rep(NA, max_arma), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "ARMA"
  class(model) <- "CHARMA"
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
  names_phi <- paste("phi", ar, sep = "")
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
  model$fitted <- ts(c(rep(NA, max_ar), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "AR"
  class(model) <- "CHARMA"
  return(model)

}
#_______________________________________________________________________________________

MA <- function(y, ma, tau){
  log_y <- log(y)
  max_ma <- max(ma)
  n <- length(y)
  n_fit <- n - max_ma
  n_ma <- length(ma)
  y_cut<- y[(max_ma + 1): n]
  log_y_cut <- log_y[(max_ma + 1): n]

  mat <- matrix(rep(1, n_fit))
  opt <- stats::lm.fit(mat, log_y_cut)
  mqo <- stats::coef(opt)

  #___________________________________
  names_theta <- paste("theta", ma, sep = "")
  names_par <- c("intercept", names_theta, "lambda")

  opt <- stats::optim(c(mqo, rep(0, n_ma), 0.7),
                      chen::ll_MA,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ma = n_ma, max_ma = max_ma,
                      ma = ma, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <-opt$par
  names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- theta <- coef[2:(max_ma + 1)]
  model$lambda <- lambda <- coef[(max_ma + 2)]
  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_ma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_ma + 1):n){
    etahat[i] <- beta0 + (theta %*% errorhat[i - ma])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_ma + 1):n])
  model$fitted <- ts(c(rep(NA, max_ma), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "MA"
  class(model) <- "CHARMA"
  return(model)

}
