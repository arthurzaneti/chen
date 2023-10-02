#' @title The Cumulative Distribution Function of the Reparameterized Chen Distribution
#'
#' @description
#' The mathematical function that describes the cumulative distribution for the Reparameterized
#' Chen distribution. Its formula is \deqn{F(y | \lambda, \mu, \tau)= 1 - \exp
#' \left[ \dfrac{\log(1-\tau)}{(1-\exp(\mu^{\lambda}))} \left(  1-\exp\left(y^{\lambda}\right)
#' \right) \right] , \quad  y > 0}
#'
#' @param y A numeric. The input of the mathematical function
#' @param theta  A length 2 numeric vector, or coercible to vector using \code{as.vector(unlist(theta))},
#'  with the parameters.
#' @param tau The quantile.
#' @param lower_tail If true, than probabilities are \eqn{P(Y \leq y)}, otherwise \eqn{P(Y \geq y)}
#' @return A double representing the cumulative probability till \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the pdf_chen_rpr function
#' cdf_chen_rpr(1, theta = c(0.7, 7), 0.3)
#' curve(cdf_chen_rpr(x, theta = c(0.5, 2), tau = 0.3),
#'       from = 0, to = 20, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen_rpr(x, theta = c(0.9, 3)),
#'       from = 0, to = 20, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen_rpr(x, theta = c(0.7, 11)),
#'       from = 0, to = 20, xlab = "y", ylab = "cdf", n = 1000)
#' curve(cdf_chen_rpr(x, theta = c(0.2, 8), tau = 0.2),
#'       from = 0, to = 20, xlab = "y", ylab = "cdf", n = 1000)




cdf_chen_rpr <- function(y, theta, tau = 0.5, lower_tail = T){
  checkmate::assert_numeric(y, lower = 0)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, lower = 0)
  checkmate::assert_true(length(theta) == 2 || length(theta) == length(y) + 1)
  checkmate::assert_number(tau, lower = 0, upper = 1)

  #__________________________________end_checks_________________________________

  lambda <- theta[1]
  mu <- theta[2 : length(theta)]
  if(lower_tail) return(1 - exp((log(1 - tau) / (1 - exp(mu ^ lambda))) * (1 - exp(y ^ lambda))))
  else return(exp((log(1 - tau) / (1 - exp(mu ^ lambda))) * (1 - exp(y ^ lambda))))
}
