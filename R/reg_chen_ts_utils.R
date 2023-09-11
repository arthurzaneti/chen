#' Determines the ARMA case
#'
#' Simply that determines the ARMA case between ARMA, AR, MA, REG_ARMA, REG_MA and
#' REG_AR. This is an internal function.
#'
#' @param isar A logical indicating if the model incorporates auto regression
#' @param isma A logical indicating if the model incorporates moving averages
#' @param isreg A logical indicating if the model incorporates regular regression
#' (with covariables)
#'
#' @return
#' A string with the ARMA case
#' @keywords internal
#' @export
#'
#' @examples
#' # No need for examples in internal function
#'
arma_case <- function(isar, isma, isreg){
  if(!isreg){
    if(isar && isma) case <- "ARMA"
    else if(isar) case <- "AR"
    else if(isma) case <- "MA"
    else stop("No values for ar or ma provided")

  }else{
    if(isar && isma) case <- "REG_ARMA"
    else if(isar) case <- "REG_AR"
    else if(isma) case <- "REG_MA"
    else stop("No values for ar or ma provided")
  }
  return(case)
}
#________________________________________________________________________________________________
#' @describeIn ll_REG_ARMA For the ARMA case
#' @order 4
ll_ARMA <- function(y, y_cut, log_y, theta, n, n_ar, n_ma, ar, ma, max_arma, tau){
  beta0 <- theta[1]
  phi <- theta[2:(n_ar + 1)]
  rho <- theta[(n_ar + 2):(n_ar + n_ma + 1)]
  lambda <- theta[n_ar + n_ma + 2]
  error <- eta <- numeric(n)

  # Generating the mus
  for(i in (max_arma + 1):n) {
    eta[i] <- beta0 + (phi %*% log_y[i - ar]) + (rho %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_arma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________
#' @describeIn ll_REG_ARMA For the AR case
#' @order 5
ll_AR <- function(y, y_cut, log_y, theta, n, n_ar, ar, max_ar, tau){
  beta0 <- theta[1]
  phi <- theta[2:(n_ar + 1)]
  lambda <- theta[n_ar + 2]
  error <- eta <- numeric(n)

  # Generating the mus
  for(i in (max_ar + 1):n) {
    eta[i] <- beta0 + (phi %*% log_y[i - ar])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_ar + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________
#' @describeIn ll_REG_ARMA For the ARMA case
#' @order 6
ll_MA <- function(y, y_cut, log_y, theta, n, n_ma, ma, max_ma, tau){
  beta0 <- theta[1]
  rho <- theta[2:(n_ma + 1)]
  lambda <- theta[n_ma + 2]
  error <- eta <- numeric(n)

  # Generating the mus
  for(i in (max_ma + 1):n) {
    eta[i] <- beta0 + (rho %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }

  mus <- exp(eta[(max_ma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________

#' Log-likelihood functions for time series
#'
#' All of the Log-likelihood functions used in reg_chen_ts. Used for optimization
#' with \code{stats::optim}. These are internal functions.
#'
#' @param y All of the output variables
#' @param y_cut \code{y} but without the first variables that can't be used for fitting
#' @param log_y log of y
#' @param theta The parameters supplied by \code{stats::optim}
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

ll_REG_ARMA <- function(y, y_cut, log_y, theta, n, n_ar, n_ma, ar, ma, max_arma, cvar, tau){
  l <- length(theta) # auxiliary variable for the indices
  beta0 <- theta[1]
  phi <- theta[2:(n_ar + 1)]
  rho <- theta[(n_ar + 2):(n_ar + n_ma + 1)]
  betas <- theta[(n_ar + n_ma + 2): (l - 1)]
  lambda <- theta[l]
  error <- eta <- numeric(n)
  # Generating the mus
  for(i in (max_arma + 1):n) {
    eta[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas)))) +
      (rho %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_arma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________

#' @describeIn ll_REG_ARMA For the REG_AR case
#' @order 2

ll_REG_AR <- function(y, y_cut, log_y, theta, n, n_ar, ar, max_ar, cvar, tau){
  l <- length(theta) # auxiliary variable for the indices
  beta0 <- theta[1]
  phi <- theta[2:(n_ar + 1)]
  betas <- theta[(n_ar + 2): (l - 1)]
  lambda <- theta[l]
  error <- eta <- numeric(n)
  # Generating the mus
  for(i in (max_ar + 1):n) {
    eta[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas)   +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas))))
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_ar + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}
#______________________________________________________________________________________________________

#' @describeIn ll_REG_ARMA For the REG_MA case
#' @order 3
ll_REG_MA <- function(y, y_cut, log_y, theta, n, n_ma, ma, max_ma, cvar, tau){
  l <- length(theta) # auxiliary variable for the indices
  beta0 <- theta[1]
  rho <- theta[2:(n_ma + 1)]
  betas <- theta[(n_ma + 2): (l - 1)]
  lambda <- theta[l]
  error <- eta <- numeric(n)
  # Generating the mus
  for(i in (max_ma + 1):n) {
    eta[i] <- beta0 +  cvar[i, ] %*% as.matrix(betas) +
      (rho %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_ma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}
