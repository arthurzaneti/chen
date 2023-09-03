#' Title
#'
#' @param isma
#' @param isar
#' @param isreg
#'
#' @return
#' @export
#'
#' @examples
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

log_lik_ARMA <- function(y, y_cut, log_y, theta, n, n_ar, n_ma, ar, ma, max_arma){
  beta0 <- theta[1]
  phi <- theta[2:(n_ar + 1)]
  rho <- theta[(n_ar + 2):(n_ar + n_ma + 1)]
  lambda <- theta[n_ar + n_ma + 2]

  error <- eta <- numeric(n)

  for(i in (max_arma + 1):n) { # Don't really know if we can avoid the for loop in here, I use the indice in to many places
    eta[i] <- beta0 + (phi%*%ynew[i - ar]) + (theta%*%error[i - ma])
    error[i] <- ynew[i] - eta[i]
  }

  mus <- exp(eta[(max_arma + 1) : n])
  sum(chen::pdf_chen_rpr(y_cut, c(lambda, mus), tau))


}
