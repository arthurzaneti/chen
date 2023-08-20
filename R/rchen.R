#' @title Random generation for the chen distribution
#'
#' @param n number of random values to generate
#' @param theta The vector of parameters. If the vector has size 2 the values
#' will be generated according to the standard Chen probaiblity density function.
#' If the vector has length 3 the values will be generated according to the
#' reparameterized Chen distribution.
#' @return A length n numeric vector.
#' @importFrom stats runif
#' @export
#'
#' @examples
#' rchen(10, c(0.7, 0.4))
#' rchen(10, c(0.1, 0.1))
#' rchen(10, c(1, 0.3))
#' hist(rchen(100, c(0.7, 0.01)))
#'
rchen <- function(n, theta){
  stopifnot(is.numeric(n), length(n) == 1)
  stopifnot(class(theta) == "numeric" || class(theta) == "list")
  if(class(theta) == "numeric"){
    stopifnot(all(theta) > 0)
    stopifnot(length(theta) == 2 || length(theta) == 3)
  }
  else if(class(theta) == "list"){
    stopifnot(length(theta) == 3)
    stopifnot(is.numeric(theta[[2]]))
    stopifnot(length(theta[[2]]) == n)
  }

  #_____________________________________________________________________________
    lambda <- theta[[1]]
    rquantiles <- stats::runif(n)
    if(length(theta) == 2){ # not reparameterized

      delta <- theta[2]
      rvalues <- log((1-(log(1-rquantiles))/delta))^(1/lambda)
      return (rvalues)

    }else{ # reparameterized
        mu <- theta[[2]]
        tau <- theta[[3]]
      rvalues <- (log(1 - log(1-rquantiles)*
                        ((1-exp(mu^lambda))/log(1-tau))))^(1/lambda)
      return(rvalues)
    }
}


