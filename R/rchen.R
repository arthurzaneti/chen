#' @title Random generation for the chen distribution
#'
#' @param n number of random values to generate
#' @param theta The vector of parameters. If the vector has size 2 the values
#' will be generated according to the standard Chen probaiblity density function.
#' If the vector has length 3 the values will be generated according to the
#' reparameterized Chen distribution.
#' @return A length n numeric vector.
#' @import checkmate
#' @export
#'
#' @examples
#' rchen(10, c(0.7, 0.4))
#' rchen(10, c(0.1, 0.1))
#' rchen(10, c(1, 0.3))
#' hist(rchen(100, c(0.7, 0.01)))
#'
rchen <- function(n, theta){


  checkmate::check_int(n)
  checkmate::check_numeric(theta, min.len = 2, max.len = 3)
  stopifnot(n > 0, theta[1] > 0, theta[2] > 0)

  lambda <- theta[1]
  rquantiles <- stats::runif(n)
  if(length(theta) == 2){ # not reparameterized

    delta <- theta[2]
    rvalues <- log((1-(log(1-rquantiles))/delta))^(1/lambda)
    return (rvalues)

  }else{ # reparameterized

    mu <- theta[2]
    tau <- theta[3]
    rvalues <- (log(1 - log(1-rquantiles)*
                      ((1-exp(mu^lambda))/log(1-tau))))^(1/lambda)
    return(rvalues)
  }
}

