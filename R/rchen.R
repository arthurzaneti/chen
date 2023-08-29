#' @title Random generation for the Chen distribution
#'
#' @param n number of random values to generate
#' @param theta A length 2 vector, or coercible to vector, look at details for
#' more information about the coersion. This will be the parameters of the distribution
#' .The first one is considered to be lambda and the second one to be delta
#'
#' @return A length n numeric vector.
#' @details The coersion of theta to vector is done using `as.vector(unlist(theta))`
#' so lists and matrices will work, as long as they are length 2.
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


