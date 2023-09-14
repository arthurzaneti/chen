#' Random generation for the CHARMA model
#'
#' Generates random values following correlated through time according to the CHARMA
#' model. Usually used for simulation together with \code{reg_chen_ts}.
#'
#' @param n Number of random values to generate
#' @param intercept The formula's intercept
#' @param lambda The distribution parameter \eqn{\lambda}
#' @param ar_coef The coefficients for the auto-regressive model
#' @param ma_coef The coefficients for the moving averages model
#' @param reg_coef The coefficients for the regression model, should only be provided
#' if cvar is provided as well.
#' @param cvar The covariables.
#' @param freq Parameter for the \code{ts} object that is returned
#' @param tau The quantile
#'
#' @details The values provided as the coefficients of the model will be coerced to vector
#' using \code{as.vector(unlist(**_coef))}, if that is not possible an error will be generated.
#' That means any type coercible to vector can be used. Similarly \code{as.matrix} is used to
#' coerce cvar.
#' @importFrom stats ts
#' @import checkmate
#' @return A \code{ts} object with the generated values and the specified frequency
#' @export
#'
#' @examples
#' plot(rchen_ts(100, 1, 0.7, ar_coef = c(0.6, 0.1)))
#' plot(rchen_ts(100, 1, 0.7, ma_coef = c(0.5, 0.2)))
#' rchen_ts(100, 1, 0.7, ar_coef = c(0.5, 0.3), ma_coef = c(0.3, 0.1), tau = 0.4)
#'
rchen_ts <- function(n, intercept = 0, lambda, ar_coef = NULL, ma_coef = NULL, reg_coef = NULL, cvar = NULL, freq = 1, tau = 0.5) {
  if(!is.null(ar_coef)) tryCatch(ar_coef <- as.vector(unlist(ar_coef)) , error = function(e) stop("The value provided for ar_coef is not coercible to vector"))
  if(!is.null(ma_coef)) tryCatch(ma_coef <- as.vector(unlist(ma_coef)) , error = function(e) stop("The value provided for ma_coef is not coercible to vector"))
  if(!is.null(reg_coef)) tryCatch(reg_coef <- as.vector(unlist(reg_coef)) , error = function(e) stop("The value provided for reg_coef is not coercible to vector"))
  if(!is.null(cvar)) tryCatch(cvar <- as.matrix(cvar) , error = function(e) stop("The value provided for cvar is not coercible to matrix"))
  checkmate::assert_integerish(n, lower = 1)
  checkmate::assert_number(lambda, lower = 0)
  checkmate::assert_number(intercept)
  checkmate::assert_numeric(ar_coef, null.ok = T)
  checkmate::assert_numeric(ma_coef, null.ok = T)
  checkmate::assert_numeric(reg_coef, null.ok = T)
  checkmate::assert_integerish(freq, lower = 1)
  checkmate::assert_number(tau, lower = 0, upper = 1)

  #______________________________________ORGANIZING_______________________________________
  isar <- !chen::is_null(ar_coef)
  isma <- !chen::is_null(ma_coef)
  isreg <- !chen::is_null(reg_coef)
  if(isar) {
    ar <- 1:length(ar_coef)
    p <- length(ar)
  }
  if(isma) {
    ma <- 1:length(ma_coef)
    q <- length(ma)
  }
  if(!isma && !isar) {
    stop("No values were provided for ar_coef or ma_coef, at least one of them
         needs to be different to NULL")
  }
  case <- chen::reg_arma_case(isar, isma, isreg)
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
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]), tau)
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
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]), tau)
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
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]), tau)
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
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]), tau)
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
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]), tau)
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
      y[i] <- chen::rchen_rpr(1, c(lambda, mu[i]), tau)
      ynew[i] <- log(y[i])
      error[i]<- ynew[i] - eta[i]
    }
    return(stats::ts(y[(buffer + 1):(n + buffer)], frequency = freq))
  }
}

