#' @title The Probability Density Function of the Reparameterized Chen Distribution
#'
#' @description
#' The mathematical function that describes the probability density for the Reparameterized
#' Chen distribution. Its formula is \deqn{f(y | \lambda, \mu, \tau)=
#' \dfrac{\log(1-\tau)}{(1-\exp(\mu^{\lambda}))} \lambda  y^{\lambda - 1}
#' \exp \left[ \dfrac{\log(1-\tau)}{(1-\exp(\mu^{\lambda}))}
#' \left( 1-\exp(y{\lambda}) \right) +y^{\lambda} \right] , \quad  y > 0}
#'
#' @param y The input of the mathematical function, if it is a vector, than the return will be a vector.
#' @param theta A length 2 vector. They will be the parameters of the distribution
#' .The first one is considered to be lambda and the second one to be mu
#' @param tau The quantile.
#' @return A double, or vector of doubles, representing the probability density at \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the cdf_chen_rpr function
#' pdf_chen_rpr(1, theta = c(0.7, 7), 0.3)
#' curve(pdf_chen_rpr(x, theta = c(0.5, 2), tau = 0.3),
#'       from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.9, 3)),
#'       from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.7, 11)),
#'       from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.2, 8), tau = 0.2),
#'       from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)


pdf_chen_rpr <- function(y, theta, tau = 0.5){
  checkmate::assert_numeric(y, lower = 0)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, lower = 0)
  checkmate::assert_number(tau, lower = 0, upper = 1)
  checkmate::assert_true(length(theta) == 2 || length(theta) == length(y) + 1)

  #__________________________________end_checks_________________________________

  lambda <- theta[1]
  mu <- theta[2 : length(theta)]
  return((log(1 - tau) / (1 - exp(mu ^ lambda))) * lambda * y ^ (lambda - 1) *
    exp((log(1 - tau) / (1 - exp(mu ^ lambda))) * (1 - exp(y ^ lambda)) + y ^ lambda))
}
