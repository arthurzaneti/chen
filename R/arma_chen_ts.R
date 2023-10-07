#' CHARMA fitting without regressors
#'
#' Used for fitting the Chen auto-regressive moving averages model with regressors. The model is given by
#' \deqn{\eta_t = \beta_{0} +
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
#' @return An object of class \code{CHARMA} with the following attributes:
#' \describe{
#'
#'   \item{\code{coef}}{The coefficients of the regression model. This is what is gonna
#'   be used for predicting new data.}
#'
#'   \item{\code{beta0}, \code{phi}, \code{theta}}{Explained in description}
#'
#'   \item{\code{lambda}}{The predicted value of the \eqn{\lambda} parameter in
#'   reparameterized Chen distribution formula.}
#'
#'   \item{\code{hessian}}{A matrix giving the estimate of the Hessian at the solutions found}
#'
#'   \item{\code{fitted}}{The output variables used for prediction, will be a column of \code{data}}
#'
#'   \item{\code{etahat}}{The predicted variables, they are in log scale}
#'
#'   \item{\code{errorhat}}{The errors in prediction, they are in log scale}
#'
#'   \item{\code{case}}{The model case between, ARMA, AR and MA, mainly used internally}
#'
#'   \item{\code{call}}{The matched function call.}}
#'
#' @export
#'
#' @examples
#' arma_chen_ts(chen::rchen_ts(100, 1, 0.7, ar_coef = c(0.5, 0.2), ma_coef = c(0.2, 0.1)), ar = 1:2, ma = 1:2)
#' arma_chen_ts(chen::rchen_ts(100, 1, 0.7, ar_coef = c(0.5, 0.2)), ar = 1:2)
#' arma_chen_ts(chen::rchen_ts(100, 1, 0.7, ma_coef = c(0.2, 0.1)), ma = 1:2)
arma_chen_ts <- function(y, ar = NULL, ma = NULL, tau = 0.5){
  tryCatch(y <- as.vector(y),
           error = function(e) stop("The object provided as y is not coercible to vector nor is a vector"))
  if(!is.null(ar)) tryCatch(ar <- as.vector(unlist(ar)) , error = function(e) stop("The value provided for ar is not coercible to vector"))
  if(!is.null(ma)) tryCatch(ma <- as.vector(unlist(ma)) , error = function(e) stop("The value provided for ma is not coercible to vector"))
  checkmate::assert_numeric(y)
  checkmate::assert_numeric(ar, null.ok = T)
  checkmate::assert_numeric(ma, null.ok = T)
  checkmate::assert_number(tau, lower = 0, upper = 1)
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
  names_par <- c("beta0", names_phi, names_theta, "lambda")

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
  model$names <- names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- phi <- coef[2:(n_ar + 1)]
  model$theta <- theta <- coef[(n_ar + 2): (n_ar + n_ma + 1)]
  model$lambda <- lambda <- coef[(n_ar + n_ma + 2)]
  model$y <- y
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
  model$tau <- tau
  #______Variance_analysis________
  model$vcov <- solve(-opt$hessian)
  colnames(model$vcov) <- rownames(model$vcov) <- c(model$names)
  diag_error <- diag(model$vcov)
  model$stderror <- sqrt(diag_error)
  #______Hipothesis_testing______
  model$zstat <- abs(c(model$coef) / model$stderror)
  model$pvalues <- 2 * (1 - stats::pnorm(model$zstat))
  #___________Metrics____________
  metrics <- list()
  model$residuals <- stats::qnorm(chen::cdf_chen_rpr(y[3:length(y)], list(model$lambda, muhat), tau))
  num_par <- length(model$coef)
  metrics$aic <- - 2 * (opt$value * (n / (n - max_arma))) + 2 * (num_par)
  metrics$bic <- - 2 * (opt$value * (n / (n - max_arma))) + log(n) * (num_par)
  metrics$hq <- - 2 * (opt$value * (n / (n - max_arma))) + log(log(n)) * (num_par)
  model$metrics <- metrics
  #______________________________
  model$intern <- list(case = "ARMA", ar = ar, ma = ma, max = max_arma)

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
  names_par <- c("beta0", names_phi, "lambda")

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
  model$names <- names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- phi <- coef[2:(n_ar + 1)]
  model$lambda <- lambda <- coef[(n_ar + 2)]
  model$y <- y

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
  model$tau <- tau
  #______Variance_analysis________
  model$vcov <- solve(-opt$hessian)
  colnames(model$vcov) <- rownames(model$vcov) <- c(model$names)
  diag_error <- diag(model$vcov)
  model$stderror <- sqrt(diag_error)
  #______Hipothesis_testing______
  model$zstat <- abs(c(model$coef) / model$stderror)
  model$pvalues <- 2 * (1 - stats::pnorm(model$zstat))
  #___________Metrics____________
  metrics <- list()
  num_par <- length(model$coef)
  model$residuals <- stats::qnorm(chen::cdf_chen_rpr(y[3:length(y)], list(model$lambda, muhat), tau))
  metrics$aic <- - 2 * (opt$value * (n / (n - max_ar))) + 2 * (num_par)
  metrics$bic <- - 2 * (opt$value * (n / (n - max_ar))) + log(n) * (num_par)
  metrics$hq <- - 2 * (opt$value * (n / (n - max_ar))) + log(log(n)) * (num_par)
  model$metrics <- metrics
  #______________________________
  model$intern <- list(case = "AR", ar = ar, max_arma = max_ar)

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
  names_par <- c("beta0", names_theta, "lambda")

  opt <- stats::optim(c(mqo, rep(0, n_ma), 0.7),
                      chen::ll_MA,
                      y = y, y_cut = y_cut, log_y = log_y, n = n,
                      n_ma = n_ma, max_ma = max_ma,
                      ma = ma, tau = tau,
                      hessian = T,
                      method = "BFGS",
                      control = list(fnscale = -1))

  model <- list()
  coef <- opt$par
  model$names <- names(coef) <- names_par
  model$coef <- coef
  model$beta0 <- beta0 <- coef[1]
  model$phi <- theta <- coef[2:(n_ma + 1)]
  model$lambda <- lambda <- coef[(n_ma + 2)]
  model$y <- y

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
  model$tau <- tau
  #______Variance_analysis________
  model$vcov <- solve(-opt$hessian)
  colnames(model$vcov) <- rownames(model$vcov) <- c(model$names)
  diag_error <- diag(model$vcov)
  model$stderror <- sqrt(diag_error)
  #______Hipothesis_testing______
  model$zstat <- abs(c(model$coef) / model$stderror)
  model$pvalues <- 2 * (1 - stats::pnorm(model$zstat))
  #___________Metrics____________
  metrics <- list()
  model$residuals <- stats::qnorm(chen::cdf_chen_rpr(y[3:length(y)], list(model$lambda, muhat), tau))
  num_par <- length(model$coef)
  metrics$aic <- - 2 * (opt$value * (n / (n - max_ma))) + 2 * (num_par)
  metrics$bic <- - 2 * (opt$value * (n / (n - max_ma))) + log(n) * (num_par)
  metrics$hq <- - 2 * (opt$value * (n / (n - max_ma))) + log(log(n)) * (num_par)
  model$metrics <- metrics
  #______________________________
  model$intern <- list(case = "MA", ma = ma, max_arma = max_ma)

  class(model) <- "CHARMA"
  return(model)

}
