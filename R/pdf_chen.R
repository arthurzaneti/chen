#' @title The Chen Probability Density Function
#'
#' @description
#' The mathematical function that describes the probability density for the Chen
#'  distribution. Its formula is \deqn{f(y|\lambda, \delta)= \delta\lambda
#'  y^{\lambda - 1} \exp \left\lbrace \delta \left[ 1-\exp(y^{\lambda})\right]
#'  +y^{\lambda} \right\rbrace  , \quad y>0.}Usually used for plotting.
#'
#' @param y A numeric
#' @param theta A length 2 numeric vector with the parameters.
#'
#' @return A double representing the probability density at \code{y}
#' @export
#'
#' @examples
#' curve(pdf_chen(x, c(0.7, 0.1)), from = 0, to = 10, xlab = "y",
#'   ylab = "Probability density")
pdf_chen <- function(y, theta){
  lambda <- theta[1]
  delta <- theta[2]
  pdf <- delta * lambda * (y ^ (lambda - 1)) *
    exp(delta * (1 - exp(y ^ lambda)) + y ^ lambda)
}
