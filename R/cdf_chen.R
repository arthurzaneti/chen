#' @title The Cumulative Distribution Function of the Chen Distribution
#'
#' @description
#' The mathematical function that describes the cumulative probability for the Chen
#'  distribution. Its formula is \deqn{F(y| \lambda,\delta)= 1 - \exp \left[  \delta \left( 1-\exp(y^{\lambda}) \right) \right]}
#'
#' @param y The input of the mathematical function, if it is a vector, than the return will be a vector.
#' @param theta A length 2 numeric vector, or coercible to vector using \code{as.vector(unlist(theta))},
#'  with the parameters.
#' @param lower_tail If true, than probabilities are \eqn{P(Y \leq y)}, otherwise \eqn{P(Y \geq y)}
#' @return A double, or vector of doubles, representing the cumulative probability till \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the pdf_chen function
#' cdf_chen(1, theta = c(0.7, 0.2))
#' curve(cdf_chen(x, theta = c(0.4, 0.1)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(0.7, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(1, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen(x, theta = c(1, 0.0001)),
#'       from = 0, to = 100, xlab = "y", ylab = "cdf", n = 1000)

cdf_chen <- function(y, theta, lower_tail = T){
  checkmate::assert_numeric(y, lower = 0)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, len = 2, lower = 0)
  #__________________________________end_checks_________________________________
  lambda <- theta[1]
  delta <- theta[2]
  if(lower_tail) return(1 - exp(delta * (1 - exp(y ^ lambda))))
  else return(exp(delta * (1 - exp(y ^ lambda))))
}
