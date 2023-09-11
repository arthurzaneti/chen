#' Log-likelihood function for the Chen distribution
#'
#' @description \deqn{
#' \ell(\theta | y) = \log(\log(1 - \tau)) - \log(1 - \exp(\mu_t^{\lambda})) +
#' (\lambda - 1) \log(y_t) + \log(\\lambda) + \frac{\log(1 - \tau)(1 - \exp(y_t^{\lambda}))}
#' {1 - \exp(\mu_t^{\lambda})} + y_t^{\lambda}
#' }
#'
#' Where:
#' - \code{\mu_t} is a parameter,
#' - \code{\lambda} is another parameter,
#' - \code{\tau} is yet another parameter, and
#' - \code{y_t} is the input variable.
#'
#' @param y
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
ll_chen <- function(y, theta){

  lambda <- theta[1]
  delta <- theta[2]
  n <- length(y)
  ll <- n * log(delta) + n * log(lambda) +
    sum((lambda - 1) * log(y) + delta * (1 - exp(y^lambda)) + y^lambda)
  return(ll)
}
