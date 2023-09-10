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
#' @import checkmate
#' @return A time series object with the generated values
#' @export
#'
#' @examples
#' plot(rchen_ts(100, 1, 0.7, ar_coef = c(0.6, 0.1)))
#' plot(rchen_ts(100, 1, 0.7, ma_coef = c(0.5, 0.2)))
#' rchen_ts(100, 1, 0.7, ar_coef = c(0.5, 0.3), ma_coef = c(0.3, 0.1))
#'
rchen_ts <- function(n, intercept, lambda, ar_coef = NULL, ma_coef = NULL, reg_coef = NULL, cvar = NULL, freq = 1) {
  if(!is.null(cvar)) tryCatch(cvar <- as.matrix(cvar) , error = function(e) stop("The value sent for cvar is not coercible to matrix"))
  checkmate::assert_integerish(n, lower = 1)
  checkmate::assert_number(lambda, lower = 0)
  checkmate::assert_numeric(ar_coef, null.ok = T)
  checkmate::assert_numeric(ma_coef, null.ok = T)
  checkmate::assert_numeric(reg_coef, null.ok = T)
  checkmate::assert_integerish(freq, lower = 1)

  #______________________________________ORGANIZING_______________________________________
  if(!is.null(ar_coef)) {
    ar <- 1:length(ar_coef)
    p <- length(ar)
  }
  if(!is.null(ma_coef)) {
    ma <- 1:length(ma_coef)
    q <- length(ma)
  }

  case <- chen::arma_case(!is.null(ar_coef), !is.null(ma_coef), !is.null(reg_coef))
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
      ynew[i] <- log(y[i])

    }
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq) )
  }

  #_________________________________________MA____________________________________________
  else if(case == "MA") {

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
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq))
  }
  #______________________________________REG ARMA_________________________________________
  else if(case == "REG_ARMA"){

    maxx = max(p, q)

    ynew <-rep(intercept, (n + buffer))
    mu <- exp(ynew)
    # We have a problem with generating a buffer in the regression case
    # We only have n rows of covariables, but we need to generate n + buffer values
    # My idea was to add buffer rows to the beggining of the cvar matrice with the mean of each column,
    # Really just a choice here, don't know if it makes any sense
    cvar_buffered <- rbind(matrix(rep(colMeans(cvar), buffer),
                                  nrow = buffer,
                                  byrow = T),
                           cvar)

    error <- rep(0, n + buffer) # E(error)=0
    eta <- y <- NULL
    for(i in (maxx + 1):(n + buffer))
    {
      eta[i] <- intercept + (ar_coef %*% (ynew[i - ar])) + (ma_coef %*% error[i - ma]) + (reg_coef %*% cvar_buffered[i, ])
      mu[i] <- exp(eta[i])
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]))
      ynew[i] <- log(y[i])
      error[i] <- ynew[i] - eta[i]
    }
    return(stats::ts(y[(buffer + 1) : (n + buffer)], frequency = freq) )
  }
  #_________________________________________REG_AR________________________________________

  else if(case == "REG_AR"){

    ynew <- rep(intercept, (n + buffer))
    mu <- exp(ynew)
    cvar_buffered <- rbind(matrix(rep(colMeans(cvar), buffer),
                                  nrow = buffer,
                                  byrow = T),
                           cvar)

    eta <- y <- NULL

    eta <- intercept + (ar_coef%*%(ynew[(p+1)-ar]))
    muu <- exp(eta)

    for(i in (p+1):(n+buffer))
    {
      eta[i] <- intercept + (ar_coef %*% (ynew[i - ar])) + (reg_coef %*% cvar_buffered[i, ])
      mu[i] <- exp(eta[i])
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]))
      ynew[i] <- log(y[i])

    }
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq) )
  }
  #_________________________________________REG_MA________________________________________

  else if(case == "REG_MA") {

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
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq))
  }
}

