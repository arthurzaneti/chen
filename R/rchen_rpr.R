#' @title Random generation for the Reparameterized Chen distribution
#'
#' @description Used for generating random variables. Specially usefull for simulation
#' purposes.
#'
#' @param n Number of random values to generate
#' @param theta A length 2 or n+1 vector or list with lambda and mu respectively.
#' The values will be coersed to vector using \code{as.vector(unlist(theta))} and
#' lambda will always be considered as the first element in that vector, if the length is two than all values will
#' be generated with the same mu, if the length is n+1 than one value will be generated with each mu.
#' @param tau The quantile.
#'
#' @return A length n numeric vector with the random values.
#'
#' @importFrom stats runif
#' @import checkmate
#' @export
#'
#' @examples
#' rchen_rpr(10, c(0.7, 7), 0.5)
#' rchen_rpr(10, c(0.1, 1))
#' rchen_rpr(100, list(0.7, stats::runif(100, 6,7)), .3)
#' hist(rchen_rpr(100, c(0.7, 7), 0.7))
#'
rchen_rpr <- function(n, theta, tau = 0.5){
  checkmate::assert_number(n, lower = 1)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, lower = 0)
  checkmate::assert_true(length(theta) == 2 || length(theta) == (n+1))
  checkmate::assert_number(tau, lower = 0, upper = 1)

  #_____________________________________________________________________________
  lambda <- theta[1]
  mus <- theta[2: length(theta)]
  rquantiles <- stats::runif(n)
  return((log(1 - log(1 - rquantiles) * ((1 - exp(mus ^ lambda)) / log(1 - tau)))) ^ (1 / lambda))
}
