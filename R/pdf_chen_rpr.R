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
#' @param theta A length 2 numeric vector with the parameters.
#' @param tau The quantile
#' @return A double representing the probability density at \code{y}
#' @export
#'
#' @examples
#' #same numbers as the cdf_chen_rpr function
#' curve(pdf_chen_rpr(x, theta = c(0.7, 7), tau = 0.5), from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.5, 2), tau = 0.3), from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.9, 3)), from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.7, 11)), from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)
#' curve(pdf_chen_rpr(x, theta = c(0.2, 8), tau = 0.2), from = 0, to = 20, xlab = "y", ylab = "pdf", n = 1000)


pdf_chen_rpr <- function(y, theta, tau = 0.5){
  stopifnot(
    "y must be a numeric vector" = is.numeric(y),
    "Theta must be a numeric vector" = is.numeric(theta),
    "Theta must have a length of 2" = length(theta) == 2,
    "Tau must me a single number" = length(tau) == 1 || is.numeric(tau)
  )

  #__________________________________end_checks_________________________________

  lambda <- theta[1]
  mu <- theta[2]
  pdf_rpr <- (log(1 - tau) / (1 - exp(mu ^ lambda))) * lambda * y ^ (lambda - 1) *
    exp((log(1 - tau) / (1 - exp(mu ^ lambda))) * (1 - exp(y ^ lambda)) + y ^ lambda)
}
