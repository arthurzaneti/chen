#' @title Parameter estimation for the Chen distribution
#' @description
#' The Maximum likelihood estimate for the pair of parameters is numerically approximated with optim from the stats package. The estim_chen function is effectivelly a wrapper for optim, but it defines and uses the Chen log-likelihood function as well.
#'
#'
#' @param y Values sampled from a Chen distribution.
#'
#' @return The best set of parameters found
#' @export
#'
#' @examples
#' estim_chen(rchen(10, c(1,1)))
#'
estim_chen <- function(y){
  estim <- stats::optim(par=c(1,1),
                        fn=ll_chen,
                        y=y,
                        method ="L-BFGS-B",
                        hessian = T,
                        lower = c(0, 0),
                        control = list(fnscale=-1))
  return(estim$par)
}

ll_chen <- function(y, theta){ #maximum likelihood function
  lambda <- theta[1]
  delta <- theta[2]
  n <- length(y)
  MLF <- n * (2 * log(delta) + log(lambda)) +
    sum((lambda-1) * log(y) - delta * exp(y ^ lambda) + y ^lambda)
}
