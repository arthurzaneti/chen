#' @title The Probability Density Function of the Reparameterized Chen Distribution
#'
#' @description
#' The mathematical function that describes the probability density for the Reparameterized
#' Chen distribution. Its formula is \deqn{f(y | \lambda, \mu, \tau)=
#' \dfrac{\log(1-\tau)}{(1-\exp(\mu^{\lambda}))} \lambda  y^{\lambda - 1}
#' \exp \left[ \dfrac{\log(1-\tau)}{(1-\exp(\mu^{\lambda}))}
#' \left( 1-\exp(y{\lambda}) \right) +y^{\lambda} \right] , \quad  y > 0}
#'
#' @param y A numeric. The input of the mathematical function
#' @param theta A length 2 numeric vector, or coercible to vector using `as.vector(unlist())`,
#'  with the parameters.
#' @param tau A single number between 0 and 1 that is the quantile.
#' @return A double representing the probability density at \code{y}
#' @import checkmate
#' @export
#'
#' @examples
#' # same numbers as the pdf_chen_rpr function
#' curve(pdf_chen_rpr(x, theta = c(0.7, 7), tau = 0.5),
#'       from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
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
  checkmate::assert_numeric(theta, len = 2, lower = 0)
  checkmate::assert_number(tau, lower = 0, upper = 1)

  #__________________________________end_checks_________________________________

  lambda <- theta[1]
  mu <- theta[2]
  return((log(1 - tau) / (1 - exp(mu ^ lambda))) * lambda * y ^ (lambda - 1) *
    exp((log(1 - tau) / (1 - exp(mu ^ lambda))) * (1 - exp(y ^ lambda)) + y ^ lambda))
}
