#' @title The Cumulative Distribution Function of the Chen Distribution
#'
#' @description
#' The mathematical function that describes the cumulative probability for the Chen
#'  distribution. Its formula is \deqn{F(y| \lambda,\delta)= 1 - \exp \left[  \delta \left( 1-\exp(y^{\lambda}) \right) \right].}.
#'
#' @param y A numeric. The input of the mathematical function
#' @param theta A length 2 numeric vector with the parameters.
#'
#' @return A double representing the cumulative probability till y \code{y}
#' @export
#'
#' @examples
#' curve(pdf_chen(x, c(0.7, 0.1)), from = 0, to = 10, xlab = "y",
#'   ylab = "Probability density")
cdf_chen <- function(y, theta){
  #__________________________________end_checks_________________________________
  lambda <- theta[1]
  delta <- theta[2]
  return(1 - exp(delta * (1 - exp(y ^ lambda))))
}
