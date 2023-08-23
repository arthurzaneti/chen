#' @title The Cumulative Distribution Function of the Chen Distribution
#'
#' @description
#' The mathematical function that describes the cumulative probability for the Chen
#'  distribution. Its formula is \deqn{F(y| \lambda,\delta)= 1 - \exp \left[  \delta \left( 1-\exp(y^{\lambda}) \right) \right].}.
#'
#' @param y A numeric. The input of the mathematical function
#' @param theta A length 2 numeric vector, or coercible to vector using `as.vector(unlist())`,
#'  with the parameters.
#'
#' @return A double representing the cumulative probability till y \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the cdf_chen function
#' curve(cdf_chen(x, theta = c(0.4, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(0.4, 0.1)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(0.7, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(1, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(1, 0.0001)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)

cdf_chen <- function(y, theta){
  checkmate::assert_numeric(y, lower = 0)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, len = 2, lower = 0)
  #__________________________________end_checks_________________________________
  lambda <- theta[1]
  delta <- theta[2]
  cdf <- 1 - exp(delta * (1 - exp(y ^ lambda)))
}
