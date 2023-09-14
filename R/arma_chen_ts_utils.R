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
reg_arma_case <- function(isar, isma, isreg){
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
#' Log-likelihood functions for the time series model with no regressors
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
ll_ARMA <- function(y, y_cut, log_y, vars, n, n_ar, n_ma, ar, ma, max_arma, tau){
  beta0 <- vars[1]
  phi <- vars[2:(n_ar + 1)]
  theta <- vars[(n_ar + 2):(n_ar + n_ma + 1)]
  lambda <- vars[n_ar + n_ma + 2]
  error <- eta <- numeric(n)

  # Generating the mus
  for(i in (max_arma + 1):n) {
    eta[i] <- beta0 + (phi %*% log_y[i - ar]) + (theta %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_arma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________
#' @describeIn ll_ARMA For the AR case
#' @order 2
ll_AR <- function(y, y_cut, log_y, vars, n, n_ar, ar, max_ar, tau){
  beta0 <- vars[1]
  phi <- vars[2:(n_ar + 1)]
  lambda <- vars[n_ar + 2]
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
#' @describeIn ll_ARMA For the ARMA case
#' @order 3
ll_MA <- function(y, y_cut, log_y, vars, n, n_ma, ma, max_ma, tau){
  beta0 <- vars[1]
  theta <- vars[2:(n_ma + 1)]
  lambda <- vars[n_ma + 2]
  error <- eta <- numeric(n)

  # Generating the mus
  for(i in (max_ma + 1):n) {
    eta[i] <- beta0 + (theta %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }

  mus <- exp(eta[(max_ma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr(y_cut, c(lambda, mus), tau))
}




