#' Log-likelihood function for the reparameterized Chen distribution
<<<<<<< HEAD
=======
#'
#' @description Mathematical formula given by \deqn{
#' \ell(\theta | y) = \log(\log(1 - \tau)) - \log(1 - \exp(\mu^{\lambda})) +
#' (\lambda - 1) \log(y_t) + \log(\lambda) + \frac{\log(1 - \tau)(1 - \exp(y_t^{\lambda}))}
#' {1 - \exp(\mu^{\lambda})} + y_t^{\lambda}
#' }
#' Where \eqn{\mu} and \eqn{\lambda} are parameters, \eqn{\tau} is quantile
#' and \eqn{y} is the random sample.
#'
#' @param y A numeric vector with the random sample
#' @param theta A length 2 vector. They will be the parameters of the distribution
#' .The first one is considered to be \eqn{\lambda} and the second one to be \eqn{\mu}
#' @param tau The quantile
#'
#' @return The output of the log-likelihood formula, that is, the likelihood of the given set
#' of parameters given the output variables.
#' @export
>>>>>>> 28da0d9 (Rebasing)
#'
#' @description Mathematical formula given by \deqn{
#' \ell(\theta | y) = \log(\log(1 - \tau)) - \log(1 - \exp(\mu^{\lambda})) +
#' (\lambda - 1) \log(y_t) + \log(\lambda) + \frac{\log(1 - \tau)(1 - \exp(y_t^{\lambda}))}
#' {1 - \exp(\mu^{\lambda})} + y_t^{\lambda}
#' }
#' Where \eqn{\mu} and \eqn{\lambda} are parameters, \eqn{\tau} is quantile
#' and \eqn{y} is the random sample.
#'
#' @param y A numeric vector with the random sample
#' @param theta A length 2 vector. They will be the parameters of the distribution
#' .The first one is considered to be \eqn{\lambda} and the second one to be \eqn{\mu}
#' @param tau The quantile
#'
#' @return The output of the log-likelihood formula, that is, the likelihood of the given set
#' of parameters given the output variables.
#' @export
#'
ll_chen_rpr <- function(y, theta, tau){
  checkmate::assert_number(n, lower = 1)
  theta <- as.vector(unlist(theta))
  checkmate::assert_numeric(theta, lower = 0)
  checkmate::assert_true(length(theta) == 2)
  checkmate::assert_number(tau, lower = 0, upper = 1)
  lambda <- theta[1]
  mu <- theta[2:length(theta)]
  n <- length(y)
  ll <- suppressWarnings(sum((log(log(1 - tau) / (1 - exp(mu^lambda))) +
                                log(lambda) + (lambda - 1) * log(y) +
                                (log(1 - tau) / (1 - exp(mu^lambda))) * (1 - exp(y^lambda)) + (y^lambda))))
  return(ll)
}
