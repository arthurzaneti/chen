#' @title Random generation for the Reparameterized Chen distribution
#'
#'
#' @param n number of random values to generate
#' @param theta A length 2 or n+1 vector or list with lambda and mu respectively.
#' The values will be coersed to vector using `as.vector(unlist(theta))` and
#' lambda will always be considered as the first element in that vector, if the length is 2 than all values will
#' be generated with the same mu, if the length is n+1 than one value will be generated with each mu. See details.
#' @param tau The quantile
#'
#' @return A length n numeric vector.
#' @details The coersion of theta to vector is done using `as.vector(unlist(theta))`
#' so lists, matrices and dataframes will work. The generation of random values with different mus
#' is usefull in the context of regression especially. If the length of the vector
#' is between 2 and n+1 the last element will be copied untill the length is n+1 and a warning will inform
#' you that the copying occured.
#' @importFrom stats runif
#' @export
#'
#' @examples
#' rchen(10, c(0.7, 0.4))
#' rchen(10, c(0.1, 0.1))
#' rchen(10, c(1, 0.3))
#' hist(rchen(100, c(0.7, 0.01)))
#'
rchen <- function(n, theta, tau){
  stopifnot(is.numeric(n), length(n) == 1, n > 0)
  theta <- as.vector(unlist(theta))
  stopifnot(is.numeric(theta))
  stopifnot(all(theta > 0))
  stopifnot(length(theta) == 2 || length(theta) == 3)

  #_____________________________________________________________________________
  lambda <- theta[1]
  delta <- theta[2]
  rquantiles <- stats::runif(n)
  log((1-(log(1-rquantiles))/delta))^(1/lambda)

}
