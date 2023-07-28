#' @title Random generation for the chen distribution with parameters lambda and delta
#'
#' @param n number of random values to generate
#' @param theta A length 2 numeric vector. The first element is the parameter lambda, and the second one is delta, both need to be bigger than 0.
#' @return A length n numeric vector.
#' @import checkmate
#' @export
#'
#' @examples
#' rchen(c(0.7, 0.4), 30)
#' rchen(c(0.1, 0.1), 10)
#' rchen(c(1, 0.3), 10)
#' rchen(c(0.7, 0.01), 10)
#'
rchen <- function(n, theta){

  checkmate::assert(checkmate::check_numeric(theta, len=2),
                    checkmate::check_int(n))

  lambda <- theta[1]
  delta <- theta[2]
  valores_aleatorios_tau <- stats::runif(n)
  valores_chen <- log((1-(log(1-valores_aleatorios_tau))/delta))^(1/lambda)
  return (valores_chen)
}
