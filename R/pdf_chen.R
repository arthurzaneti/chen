#' @title The Probability Density Function of the Chen Distribution
#'
#' @description
#' The mathematical function that describes the probability density for the Chen
#'  distribution. Its formula is \deqn{f(y|\lambda, \delta)= \delta\lambda
#'  y^{\lambda - 1} \exp \left\lbrace \delta \left[ 1-\exp(y^{\lambda})\right]
#'  +y^{\lambda} \right\rbrace  , \quad y>0}
#'
#' @param y The input of the mathematical function, if it is a vector, than the return will be a vector.
#' @param theta A length 2 vector. They will be the parameters of the distribution
#' .The first one is considered to be lambda and the second one to be delta
#'
#' @return A double, or vector of doubles, representing the probability density at \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the cdf_chen function
#' pdf_chen(1, theta = c(0.7, 0.2))
#' curve(pdf_chen(x, theta = c(0.4, 0.1)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(0.7, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(1, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(1, 0.0001)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#'
pdf_chen <- function(y, theta){
  checkmate::assert_numeric(y, lower = 0)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, lower = 0)
  #__________________________________end_checks_________________________________

  lambda <- theta[1]
  delta <- theta[2]
  return(delta * lambda * (y ^ (lambda - 1)) *
    exp(delta * (1 - exp(y ^ lambda)) + y ^ lambda))
}

