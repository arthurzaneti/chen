#' Title
#'
#' @param y
#' @param theta
#' @param tau
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
ll_chen_rpr_ts <- function(y, theta, tau){
  lambda <- theta[1]
  mu <- theta[2:length(theta)]
  n <- length(y)
  ll <- suppressWarnings(sum((log(log(1 - tau) / (1 - exp(mu^lambda))) +
                                log(lambda) + (lambda - 1) * log(y) +
                                (log(1 - tau) / (1 - exp(mu^lambda))) * (1 - exp(y^lambda)) + (y^lambda))))
  return(ll)
}
#________________________________________________________________________________________________
#' Title
#'
#' @param isma
#' @param isar
#' @param isreg
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#'
#________________________________________________________________________________________________
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

#' Title
#'
#' @param y
#' @param y_cut
#' @param log_y
#' @param theta
#' @param n
#' @param max_ar
#' @param max_ma
#' @param ar
#' @param ma
#' @param max_arma
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples

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
  sum(chen::ll_chen_rpr_ts(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________
#' Title
#'
#' @param y
#' @param y_cut
#' @param log_y
#' @param theta
#' @param n
#' @param n_ar
#' @param n_ma
#' @param ar
#' @param ma
#' @param max_arma
#' @param tau
#'
#' @return
#' @export
#'
#' @examples

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
  sum(chen::ll_chen_rpr_ts(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________
#' Title
#'
#' @param y
#' @param y_cut
#' @param log_y
#' @param theta
#' @param n
#' @param n_ma
#' @param ma
#' @param max_ma
#' @param tau
#'
#' @return
#' @export
#'
#' @examples

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
  sum(chen::ll_chen_rpr_ts(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________

#' Title
#'
#' @param y
#' @param y_cut
#' @param log_y
#' @param theta
#' @param n
#' @param max_ar
#' @param max_ma
#' @param ar
#' @param ma
#' @param cvar
#' @param max_arma
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples

ll_REG_ARMA <- function(y, y_cut, log_y, theta, n, n_ar, n_ma, ar, ma, max_arma, cvar, tau){
  beta0 <- theta[1]
  phi <- theta[2:(n_ar + 1)]
  rho <- theta[(n_ar + 2):(n_ar + n_ma + 1)]
  lambda <- theta[n_ar + n_ma + 2]
  betas <- theta[n_ar + n_ma + 3: length(theta)]
  error <- eta <- numeric(n)
  print(cvar)
  print(theta)
  # Generating the mus
  for(i in (max_arma + 1):n) {
    eta[i] <- beta0 + cvar[i, ] %*% as.matrix(betas) +
      (phi %*% (log_y[i - ar] - (cvar[i - ar, ] %*% as.matrix(betas)))) +
      (rho %*% error[i - ma])
    error[i] <- log_y[i] - eta[i]
  }
  mus <- exp(eta[(max_arma + 1) : n])

  # Evaluating the ll function
  sum(chen::ll_chen_rpr_ts(y_cut, c(lambda, mus), tau))
}

#______________________________________________________________________________________________________
