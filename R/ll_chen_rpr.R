#' Title
#'
#' @param y
#' @param theta
#' @param tau
#'
#' @return
#' @export
#'
#' @examples
ll_chen_rpr <- function(y, theta, tau){
  lambda <- theta[1]
  mu <- theta[2:length(theta)]
  n <- length(y)
  ll <- suppressWarnings(sum((log(log(1 - tau) / (1 - exp(mu^lambda))) +
                                log(lambda) + (lambda - 1) * log(y) +
                                (log(1 - tau) / (1 - exp(mu^lambda))) * (1 - exp(y^lambda)) + (y^lambda))))
  return(ll)
}
