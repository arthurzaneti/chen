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
                "ARMA" = ARMA(y, ar, ma, tau),
                "AR" = AR(y, ar, tau),
                "MA" = MA(y, ma, tau),
                "REG_ARMA" = REG_ARMA(y, ar, ma, cvar, tau),
                "REG_AR" = REG_AR(y, ar, cvar, tau),
                "REG_MA" = REG_MA(y, ma, cvar, tau)))

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
  names_rho <- paste("rho", ma, sep = "")
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
  model$fitted <- ts(c(rep(NA, max_arma), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "ARMA"
  class(model) <- "reg_CHARMA"
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
  class(model) <- "reg_CHARMA"
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
  names_rho <- paste("rho", ma, sep = "")
  names_par <- c("intercept", names_rho, "lambda")

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
  model$phi <- rho <- coef[2:(max_ma + 1)]
  model$lambda <- lambda <- coef[(max_ma + 2)]
  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_ma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_ma + 1):n){
    etahat[i] <- beta0 + (rho %*% errorhat[i - ma])
    errorhat[i] <- log_y[i] - etahat[i]
  }

  muhat <- exp(etahat[(max_ma + 1):n])
  model$fitted <- ts(c(rep(NA, max_ma), muhat), start = stats::start(y), frequency = stats::frequency(y))
  model$etahat <- etahat
  model$errorhat <- errorhat
  model$case <- "MA"
  class(model) <- "reg_CHARMA"
  return(model)

}
#_______________________________________________________________________________________
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
  names_rho <- paste("rho", ma, sep = "")
  names_betas <- paste("beta", 1:length(betas))
  names_par <- c("intercept", names_phi, names_rho, names_betas, "lambda")
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
  model$rho <- rho <- coef[(max_ar + 2): (max_ar + max_ma + 1)]
  model$betas <- coef[(max_ar + max_ma + 2) : (l - 1)]
  model$lambda <- lambda <- coef[l]

  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_arma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_arma + 1):n){
    etahat[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas)))) +
      (rho %*% errorhat[i - ma])
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

  names_rho <- paste("rho", ma, sep = "")
  names_betas <- paste("beta", 1:length(betas))
  names_par <- c("intercept", names_rho, names_betas, "lambda")
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
  model$rho <- rho <- coef[2: (n_ma + 1)]
  model$betas <- coef[(n_ma + 2) : (l - 1)]
  model$lambda <- lambda <- coef[l]

  model$hessian <- opt$hessian
  model$fitted <- c(rep(NA, max_ma), y_cut)

  errorhat <- rep(0, n)
  etahat <- rep(NA, n)

  for(i in (max_ma + 1):n){
    etahat[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (rho %*% errorhat[i - ma])
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







