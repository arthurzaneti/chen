#' Title
#'
#' @param y
#' @param ar
#' @param ma
#' @param tau
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#'
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
  class(model) <- "CHARMA_reg"
  return(model)
}
