#' @title Random generation for the Chen distribution
#'
#'
#' @param n number of random values to generate
#' @param theta A length 2 vector, or coercible to vector, look at details for
#' more information about the coersion. This will be the parameters of the distribution
#' .The first one is considered to be lambda and the second one to be delta
#'
#' @return A length n numeric vector.
#' @details The coersion of theta to vector is done using `as.vector(unlist(theta))`
#' so lists, matrices and dataframes will work, as long as they are length 2.
#' @importFrom stats runif
#' @export
#'
#' @examples
#' rchen(10, c(0.7, 0.4))
#' rchen(10, c(0.1, 0.1))
#' rchen(10, c(1, 0.3))
#' hist(rchen(100, c(0.7, 0.01)))
#'
rchen <- function(n, theta){
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


