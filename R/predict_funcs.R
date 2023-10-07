#' Prediction for the Chen regression model
#'
#' This function is used for predictions using regression models fitted by \code{reg_chen}
#'
#' @param x An object fitted by \code{reg_chen}
#' @param data A matrix with the predictors.
#' @param formula A \code{formula} object. This is an optional parameter, if no formula is provided
#' than \code{data} is considered to be the already formated matrix of covariables. It is recommended
#' to send a \code{formula} object to avoid errors.
#'
#' @return The vector of predictions.
#' @import checkmate
#' @export
#'
#' @examples
reg_chen_predict <- function(x, data, formula = NULL){
  checkmate::assert_class(x, "reg_chen")
  checkmate::assert_formula(formula, null.ok = T)

  if(!is.null(formula)) data <- data[all.vars(formula[[3]])]
  return(x$coef[1] + as.matrix(data) %*% x$coef[2:length(x$coef)])
}
#_________________________________________________________________________________________________
#' Prediction for the CHARMA model without regressors
#'
#' @param x A CHARMA model fitted by \code{arma_chen_ts}
#' @param n The number of steps in the future that will be predicted
#' @param return Specifies the type of return. If \code{"add"} than the return will be the fitted series with
#' the predictions added at the end, if \code{"basic"} than the return is just the predictions.
#'
#' @return As specified in \code{return}
#'
#' @import checkmate
#' @export
#'
#' @examples
CHARMA_predict <- function(x, n, return = "add"){
  checkmate::assert_class(x, "CHARMA")
  checkmate::assert_number(n)
  checkmate::assert_choice(return, c("add", "basic"))
  #_____________________________ORGANIZING_____________________________
  intern <- x$intern
  n_fitted <- length(x$fitted)
  n_total <- n_fitted + n
  log_y <- chen::extend_vec(log(x$fitted), n_total)
  errorhat <- chen::extend_vec(x$etahat, n_total) # not used in all cases
  log_mu <- numeric(n)
  #_____________________________CALCULATION_____________________________
  if(intern$case == "ARMA"){
    for(i in 1:n){
      log_mu[i] <- x$beta0 + (x$phi %*% log_y[n_fitted + i - intern$ar]) +
        (x$theta %*% errorhat[n_fitted + i - intern$ma])
      errorhat[n_fitted + i] <- 0
    }
  }
  else if(intern$case == "AR"){
    for(i in 1:n){
      log_mu[i] <- x$beta0 + (x$phi %*% log_y[n_fitted + i - intern$ar])
    }
  }
  if(intern$case == "MA"){
    for(i in 1:n){
      log_mu[i] <- x$beta0 + (x$theta %*% errorhat[n_fitted + i - intern$ma])
      errorhat[n_fitted + i] <- 0
    }
  }
  if(return == "add") return(c(x$fitted, chen::rchen_rpr(n, list(x$lambda, exp(log_mu), x$tau))))
  else return(chen::rchen_rpr(n, list(x$lambda, exp(log_mu), x$tau)))
}
#_______________________________________________________________________________________________________
#' Prediction for the CHARMA model with regressors
#'
#' This function is used for predictions using regression models fitted by \code{reg_CHARMA}
#'
#' @param x An object fitted by \code{reg_CHARMA}
#' @param data A matrix with the predictors.
#' @param formula A \code{formula} object. This is an optional parameter, if no formula is provided
#' than \code{data} is considered to be the already formated matrix of covariables. It is recommended
#' to provide a \code{formula} object.
#' @param return Specifies the type of return. If \code{"add"} than the return will be the fitted series with
#' the predictions added at the end, if \code{"basic"} than the return is just the predictions.
#'
#' @return As specified in \code{return}
#' @import checkmate
#' @export
#'
#' @examples
reg_CHARMA_predict <- function(x, data, formula = NULL, return = "add"){
  checkmate::assert_class(x, "reg_CHARMA")
  checkmate::assert_formula(formula, null.ok = T)
  checkmate::assert_choice(return, c("add", "basic"))
  #_______________________________________ORGANIZING______________________________________________
  if(!is.null(formula)) data <- data[all.vars(formula[[3]])]
  data <- as.matrix(data)
  n <- nrow(data)
  intern <- x$intern
  n_fitted <- length(x$fitted)
  n_total <- n_fitted + n
  log_y <- chen::extend_vec(log(x$fitted), n_total)
  errorhat <- chen::extend_vec(x$etahat, n_total) # not used in all cases
  log_mu <- numeric(n)
  betas <- x$betas
  #_______________________________________CALCULATION_____________________________________________
  if(intern$case == "REG_ARMA"){
    for(i in 1:n){
      log_mu[i] <- x$beta0 + data[i, ] %*% x$betas +
        (x$phi %*% log_y[n_fitted + i - intern$ar]) +
        (x$theta %*% errorhat[n_fitted + i - intern$ma])
      errorhat[n_fitted + i] <- 0
    }
  }
  else if(intern$case == "REG_AR"){
    for(i in 1:n){
      log_mu[i] <- x$beta0 + x$beta0 + data[i, ] %*% x$betas +
        (x$phi %*% log_y[n_fitted + i - intern$ar])
    }
  }
  else if(intern$case == "REG_MA"){
    for(i in 1:n){
      log_mu[i] <- x$beta0 + x$beta0 + data[i, ] %*% x$betas +
        (x$theta %*% errorhat[n_fitted + i - intern$ma])
      errorhat[n_fitted + i] <- 0
    }
  }
  #_________________________________________RETURN________________________________________________
  if(return == "add") return(c(x$fitted, chen::rchen_rpr(n, list(x$lambda, exp(log_mu)), x$tau)))
  else return(chen::rchen_rpr(n, list(x$lambda, exp(log_mu)), x$tau))
}
