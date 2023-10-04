#' Title
#'
#' @param x
#' @param data
#' @param formula
#'
#' @return
#' @export
#'
#' @examples
reg_chen_predict <- function(x, data, formula = NULL){
  if(!is.null(formula)) data <- data[all.vars(formula[[3]])]
  return(x$coef[1] + as.matrix(data) %*% x$coef[2:length(x$coef)])
}

#' Title
#'
#' @param x
#'
#' @return
#' @import checkmate
#' @export
#'
#' @examples
CHARMA_predict <- function(x, n, return = "add"){
  checkmate::assert_class(x, "CHARMA")
  checkmate::assert_number(n)
  checkmate::assert_choice(return, c("add", "basic"))
  #_______________________________________ORGANIZING______________________________________________
  intern <- x$intern
  n_fitted <- length(x$fitted)
  n_total <- n_fitted + n
  log_y <- chen::extend_vec(log(x$fitted), n_total)
  errorhat <- chen::extend_vec(x$etahat, n_total) # not used in all cases
  log_mu <- numeric(n)
  #_______________________________________CALCULATION_____________________________________________
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
  #_________________________________________RETURN________________________________________________
  if(return == "add") return(c(x$fitted, chen::rchen_rpr(n, list(x$lambda, exp(log_mu), x$tau))))
  else return(chen::rchen_rpr(n, list(x$lambda, exp(log_mu), x$tau)))
}

