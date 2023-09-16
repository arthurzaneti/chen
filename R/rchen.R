#' @title Random generation for the Chen distribution
#'
#' @description Used for generating random variables. Specially usefull for simulation
#' purposes.
#'
#' @param n Number of random values to generate
#' @param theta A length 2 vector. They will be the parameters of the distribution
#' .The first one is considered to be lambda and the second one to be delta
#'
#' @return A length n numeric vector with the random values.
#' @importFrom stats runif
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
  checkmate::assert_number(n, lower = 1)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, any.missing = F, len = 2, lower = 0)
  #_____________________________________________________________________________
  lambda <- theta[1]
  delta <- theta[2]
  rquantiles <- stats::runif(n)
  return(log((1-(log(1-rquantiles))/delta))^(1/lambda))
}


