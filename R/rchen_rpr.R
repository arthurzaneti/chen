#' @title Random generation for the Reparameterized Chen distribution
#'
#' @param n number of random values to generate
#' @param theta A length 2 or n+1 vector or list with lambda and mu respectively.
#' The values will be coersed to vector using `as.vector(unlist(theta))` and
#' lambda will always be considered as the first element in that vector, if the length is two than all values will
#' be generated with the same mu, if the length is n+1 than one value will be generated with each mu. See details.
#' @param tau The quantile, default is 0.5.
#'
#' @return A length n numeric vector.
#' @details The coersion of theta to vector is done using `as.vector(unlist(theta))`
#' so lists and matrices will work (dataframes to, but they are not recommended) as long as they have the correct length
#' . The generation of random values with different mus is usefull in the context of regression especially.
#'
#' @importFrom stats runif
#' @import checkmate
#' @export
#'
#' @examples
#' rchen_rpr(10, c(0.7, 7), 0.5)
#' rchen_rpr(10, c(0.1, 1))
#' rchen_rpr(10, c(1.1, 9), .3)
#' hist(rchen_rpr(100, c(0.7, 7), 0.7))
#'
rchen_rpr <- function(n, theta, tau = 0.5){
  checkmate::assert_number(n, lower = 1)
  if(!checkmate::test_int(n)){
    warning("The value provided for n is not an integer, but it was coersed to one:
                Provided: ", n, " | Used: ", as.integer(n), "\n ")
    n <- as.integer(n)
  }
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, any.missing = F, lower = 0)
  checkmate::assert_true(length(theta) == 2 || length(theta) == (n+1))
  checkmate::assert_number(tau, lower = 0, upper = 1)

  #_____________________________________________________________________________
  lambda <- theta[1]
  mus <- theta[2: length(theta)]
  rquantiles <- stats::runif(n)
  return((log(1 - log(1 - rquantiles) * ((1 - exp(mus ^ lambda)) / log(1 - tau)))) ^ (1 / lambda))
}
