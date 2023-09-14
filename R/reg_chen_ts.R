#' CHARMA fitting with regressors
#'
#' Used for fitting the Chen auto-regressive moving averages model with regressors. The model is given by
#' \deqn{\beta_{0} + \boldsymbol{x}_t^\top\boldsymbol{\beta}+
#'\sum_{j\in ar}\phi_j [\log(y_{t-j})-\boldsymbol{x}_{t-j}^\top\boldsymbol{\beta}]
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
#' @param data  A \code{data.frame} with the variables specified in \code{formula}
#' @param formula  An object of class \code{formula} which is gonna be used for fitting
#'  the model
#' @param ar Specified in description.
#' @param ma Specified in description.
#' @param tau The quantile
#'
#' @importFrom stats lm.fit coef optim start frequency
#' @return An object of class \code{reg_CHARMA} with the following attributes:
#' \describe{
#'
#'   \item{\code{coef}}{The coefficients of the regression model. This is what is gonna
#'   be used for predicting new data.}
#'
#'   \item{\code{beta0}, \code{phi}, \code{theta}, \code{betas}}{Explained in description}
#'
#'   \item{\code{lambda}}{The predicted value of the \eqn{\lambda} parameter in
#'   reparameterized Chen distribution formula.}
#'
#'   \item{\code{hessian}}{A matrix giving the estimate of the Hessian at the solutions found}
#'
#'   \item{\code{fitted}}{The output variables used for prediction, will be a column of \code{data}}
#'
#'   \item{\code{etahat}}{The predicted variables}
#'
#'   \item{\code{errorhat}}{The errors in prediction, they are in log scale}
#'
#'
#'
#'   \item{\code{tau}}{Simply the value provided as input for \code{tau}.}
#'
#'   \item{\code{formula}}{Simply the value provided as input for \code{formula}.}
#'
#'   \item{\code{y}}{The output variables used for prediction, will be a column of \code{data}.}
#'
#'   \item{\code{cvar}}{The covariables used for prediction, all columns of \code{data}}
#'
#'   \item{\code{call}}{The matched function call.}}
#' @export
#'
#' @examples

reg_chen_ts <- function(data, formula, ar = NULL, ma = NULL, tau = 0.5){
  #__________________________________ORGANIZING____________________________________

  y <- as.vector(data[, all.vars(formula[[2]])])
  cvar <- as.matrix(data[, all.vars(formula[[3]])])
  isar <- !chen::is_null(ar)
  isma <- !chen::is_null(ma)

  case <- chen::reg_arma_case(isar, isma, T)

  return(switch(case,
                "REG_ARMA" = REG_ARMA(y, ar, ma, cvar, tau),
                "REG_AR" = REG_AR(y, ar, cvar, tau),
                "REG_MA" = REG_MA(y, ma, cvar, tau)))

}
#____________________________________CASES________________________________________

REG_ARMA <- function(y, ar, ma, cvar, tau){
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

  mat <- cbind(rep(1, n_fit), mat, cvar[(max_arma + 1):n, ])
  opt <- stats::lm.fit(mat, log_y_cut)
  mqo <- stats::coef(opt)

  # mqo is used to initialize both betas and phi
  betas <- mqo[(max_ar + 2):length(mqo)]

  #___________________________________

  names_phi <- paste("phi", ar, sep = "")
  names_theta <- paste("theta", ma, sep = "")
  names_betas <- paste("beta", 1:length(betas))
  names_par <- c("(Intercept)", names_phi, names_theta, names_betas, "lambda")
  opt_start <- c(mqo[1:(max_ar + 1)], rep(0, n_ma), betas, 0.7)
  opt <- stats::optim(opt_start,
                      chen::ll_REG_ARMA,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ar = n_ar, n_ma = n_ma, ar = ar,
                      ma = ma, max_arma = max_arma, cvar = cvar, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <- opt$par
  l <- length(coef) # auxiliary variable for the indices
  names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- phi <- coef[2:(max_ar + 1)]
  model$theta <- theta <- coef[(max_ar + 2): (max_ar + max_ma + 1)]
  model$betas <- coef[(max_ar + max_ma + 2) : (l - 1)]
  model$lambda <- lambda <- coef[l]

  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_arma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_arma + 1):n){
    etahat[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas)))) +
      (theta %*% errorhat[i - ma])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_arma + 1):n])
  model$fitted <- ts(c(rep(NA, max_arma), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "REG_ARMA"
  class(model) <- "reg_CHARMA"
  return(model)
}
#________________________________________________________________________________________
REG_AR <- function(y, ar, cvar, tau){
  log_y <- log(y)
  max_ar <- max(ar)
  n <- length(y)
  n_fit <- n - max_ar
  n_ar <- length(ar)
  y_cut<- y[(max_ar + 1): n]
  log_y_cut <- log_y[(max_ar + 1): n]

  mat <- matrix(rep(NA, n_fit * n_ar), ncol = n_ar)
  indices <- 1:n_fit
  for(i in 1:length(ar)){
    mat[indices, i] <- log_y[indices + max_ar - ar[i]]
  }

  mat <- cbind(rep(1, n_fit), mat, cvar[(max_ar + 1):n, ])
  opt <- stats::lm.fit(mat, log_y_cut)
  mqo <- stats::coef(opt)

  # mqo is used to initialize both betas and phi
  betas <- mqo[(max_ar + 2):length(mqo)]

  #___________________________________

  names_phi <- paste("phi", ar, sep = "")
  names_betas <- paste("beta", 1:length(betas))
  names_par <- c("intercept", names_phi, names_betas, "lambda")
  opt_start <- c(mqo[1:(max_ar + 1)], betas, 0.7)
  opt <- stats::optim(opt_start,
                      chen::ll_REG_AR,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ar = n_ar, ar = ar, max_ar = max_ar,
                      cvar = cvar, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <- opt$par
  l <- length(coef) # auxiliary variable for the indices
  names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- phi <- coef[2:(max_ar + 1)]
  model$betas <- coef[(max_ar + 2) : (l - 1)]
  model$lambda <- lambda <- coef[l]

  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_ar), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_ar + 1):n){
    etahat[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas))))
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_ar + 1):n])
  model$fitted <- ts(c(rep(NA, max_ar), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "REG_AR"
  class(model) <- "reg_CHARMA"
  return(model)
}
#______________________________________________________________________________________
REG_MA <- function(y, ma, cvar, tau){
  log_y <- log(y)
  max_ma <- max(ma)
  n <- length(y)
  n_fit <- n - max_ma
  n_ma <- length(ma)
  y_cut<- y[(max_ma + 1): n]
  log_y_cut <- log_y[(max_ma + 1): n]

  mat <- cbind(rep(1, n_fit), cvar[(max_ma + 1):n, ])
  opt <- stats::lm.fit(mat, log_y_cut)
  mqo <- stats::coef(opt)
  betas <- mqo[2:length(mqo)]

  #___________________________________

  names_theta <- paste("theta", ma, sep = "")
  names_betas <- paste("beta", 1:length(betas))
  names_par <- c("intercept", names_theta, names_betas, "lambda")
  opt_start <- c(mqo[1], rep(0, n_ma), betas, 0.7)
  opt <- stats::optim(opt_start,
                      chen::ll_REG_MA,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ma = n_ma,
                      ma = ma, max_ma = max_ma, cvar = cvar, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <- opt$par
  l <- length(coef) # auxiliary variable for the indices
  names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$theta <- theta <- coef[2: (n_ma + 1)]
  model$betas <- coef[(n_ma + 2) : (l - 1)]
  model$lambda <- lambda <- coef[l]

  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_ma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_ma + 1):n){
    etahat[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (theta %*% errorhat[i - ma])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_ma + 1):n])
  model$fitted <- ts(c(rep(NA, max_ma), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "REG_MA"
  class(model) <- "reg_CHARMA"
  return(model)
}







