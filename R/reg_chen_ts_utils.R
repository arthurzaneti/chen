#' Log-likelihood functions for the time series with regressors
#'
#' All of the Log-likelihood functions used in reg_chen_ts. Used for optimization
#' with \code{stats::optim}. These are internal functions.
#'
#' @param y All of the output variables
#' @param y_cut \code{y} but without the first variables that can't be used for fitting
#' @param log_y log of y
#' @param vars The parameters supplied by \code{stats::optim}
#' @param n Simply \code{length(y)}
#' @param n_ar The number of terms used in the auto-regressive model. Note how this
#' is different \code{max(ar)} since ar could be just \code{c(2)}.
#' @param n_ma The number of terms used in the moving averages model
#' @param ar The vector of indices representing auto-regressive dependency
#' @param ma The vector of indices representing moving averages dependency
#' @param max_arma The longest dependency in general
#' @param cvar The covariables being used to fit the model
#' @param tau Parameter used in the mathematical formula
#'
#' @return The output of the log-likelihood formula, that is, the likelihood of the given set
#' of parameters given the output variables.
#' @keywords internal
#' @examples
#' # No need for examples in internal function
#' @order 1
#' @export
#'
#'

ll_REG_ARMA <- function(y, y_cut, log_y, vars, n, n_ar, n_ma, ar, ma, max_arma, cvar, tau){
  l <- length(vars) # auxiliary variable for the indices
  beta0 <- vars[1]
  phi <- vars[2:(n_ar + 1)]
  theta <- vars[(n_ar + 2):(n_ar + n_ma + 1)]
  betas <- vars[(n_ar + n_ma + 2): (l - 1)]
  lambda <- vars[l]
  error <- eta <- numeric(n)
  # Generating the mus
  for(i in (max_arma + 1):n) {
    eta[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas)))) +
      (theta %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_arma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________

#' @describeIn ll_REG_ARMA For the REG_AR case
#' @order 2
#' @export

ll_REG_AR <- function(y, y_cut, log_y, vars, n, n_ar, ar, max_ar, cvar, tau){
  l <- length(vars) # auxiliary variable for the indices
  beta0 <- vars[1]
  phi <- vars[2:(n_ar + 1)]
  betas <- vars[(n_ar + 2): (l - 1)]
  lambda <- vars[l]
  error <- eta <- numeric(n)
  # Generating the mus
  for(i in (max_ar + 1):n) {
    eta[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas))))
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_ar + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, list(lambda, mus), tau))
}

#______________________________________________________________________________________________________

#' @describeIn ll_REG_ARMA For the REG_MA case
#' @order 3
#' @export
ll_REG_MA <- function(y, y_cut, log_y, vars, n, n_ma, ma, max_ma, cvar, tau){
  l <- length(vars) # auxiliary variable for the indices
  beta0 <- vars[1]
  theta <- vars[2:(n_ma + 1)]
  betas <- vars[(n_ma + 2): (l - 1)]
  lambda <- vars[l]
  error <- eta <- numeric(n)
  # Generating the mus
  for(i in (max_ma + 1):n) {
    eta[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas) +
      (theta %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_ma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, list(lambda, mus), tau))
}
