#' Title
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
