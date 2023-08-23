#' @title The Probability Density Function of the Chen Distribution
#'
#' @description
#' The mathematical function that describes the probability density for the Chen
#'  distribution. Its formula is \deqn{f(y|\lambda, \delta)= \delta\lambda
#'  y^{\lambda - 1} \exp \left\lbrace \delta \left[ 1-\exp(y^{\lambda})\right]
#'  +y^{\lambda} \right\rbrace  , \quad y>0.}
#'
#' @param y A numeric. The input of the mathematical function
#' @param theta A length 2 numeric vector, or coercible to vector using `as.vector(unlist())`,
#'  with the parameters.
#'
#' @return A double representing the probability density at \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the pdf_chen function
#' curve(pdf_chen(x, theta = c(0.4, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(0.4, 0.1)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(0.7, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(1, 0.01)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen(x, theta = c(1, 0.0001)),
#'       from = 0, to = 100, xlab = "y", ylab = "pdf", n = 1000)

pdf_chen <- function(y, theta){
  checkmate::assert_numeric(y, lower = 0)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, len = 2, lower = 0)
  #__________________________________end_checks_________________________________

  lambda <- theta[1]
  delta <- theta[2]
  return(delta * lambda * (y ^ (lambda - 1)) *
    exp(delta * (1 - exp(y ^ lambda)) + y ^ lambda))
}

