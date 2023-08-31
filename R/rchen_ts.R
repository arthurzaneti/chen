#' Random generation
#'
#' @param n Number of random values
#' @param intercept The formula's intercept
#' @param lambda The distribution parameter lambda
#' @param ar_coef The coefficients for the auto-regressive model
#' @param ma_coef The coefficients for the moving averages model
#' @param freq Parameter for the time.series object that is returned
#'
#' @importFrom stats ts
#' @return A time serie object with the generated values
#' @export
#'
#' @examples
#' plot(rchen_ts(100, 1, 0.7, ar_coef = c(0.6, 0.1)))
#' plot(rchen_ts(100, 1, 0.7, ma_coef = c(0.5, 0.2)))
#' rchen_ts(100, 1, 0.7, ar_coef = c(0.5, 0.3), ma_coef = c(0.3, 0.1))
#'
rchen_ts <- function(n, intercept, lambda, ar_coef = NULL, ma_coef = NULL, freq = 1) {
  #______________________________________ORGANIZING_______________________________________
  if(!is.null(ar_coef)) {
    ar <- 1:length(ar_coef)
    p <- length(ar)
  }
  if(!is.null(ma_coef)) {
    ma <- 1:length(ma_coef)
    q <- length(ma)
  }

  if(!is.null(ar_coef) && !is.null(ma_coef)) case <- "ARMA"
  else if(!is.null(ar_coef)) case <- "AR"
  else if(!is.null(ma_coef)) case <- "MA"
  else{
    stop("No values for ar_coef or ma_coef provided")
  }
  buffer <- 50
  #_________________________________________ARMA____________________________________________
  if(case == "ARMA"){

    maxx = max(p, q)

    ynew <-rep(intercept, (n + buffer))
    mu <- exp(ynew)

    error <- rep(0, n + buffer) # E(error)=0
    eta <- y <- NULL
    for(i in (maxx + 1):(n + buffer))
    {
      eta[i] <- intercept + (ar_coef %*% (ynew[i - ar])) + (ma_coef %*% error[i - ma])
      mu[i] <- exp(eta[i])
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]))
      ynew[i] <- log(y[i])
      error[i] <- ynew[i] - eta[i]
    }
    return(stats::ts(y[(buffer + 1) : (n + buffer)], frequency = freq) )
  }

  #_________________________________________AR____________________________________________

  else if(case == "AR"){

    ynew <- rep(intercept, (n + buffer))
    mu <- exp(ynew)
    eta <- y <- NULL

    eta <- intercept + (ar_coef%*%(ynew[(p+1)-ar]))
    muu <- exp(eta)

    for(i in (p+1):(n+buffer))
    {
      eta[i] <- intercept + (ar_coef %*% (ynew[i - ar]))
      mu[i] <- exp(eta[i])
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]))
      print(y[i])
      ynew[i] <- log(y[i])

    }
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq) )
  }

  #_________________________________________MA____________________________________________
  else {

    ynew <-rep(intercept, (n + buffer))
    mu <- exp(ynew)

    eta <- y <- error <- rep(0, n + buffer)
    X <- cbind(sin(2 * pi * (1:(n + buffer)) / 50))

    for(i in (q + 1):(n + buffer))
    {
      eta[i] <- intercept + (ma_coef %*% error[i - ma])
      mu[i] <- exp(eta[i])
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]))
      ynew[i] <- log(y[i])
      error[i]<- ynew[i] - eta[i]
    }
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq) )
  }
}

