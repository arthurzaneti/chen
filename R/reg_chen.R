#' Title
#'
#' @param data A data.frame(or coercible to data.frame)
#' @param formula The formula for the regression
#' @param tau The quantile
#' @param stripped Nothing yet...
#'
#' @return The linear model to predict the values of mu
#'
#' @export
#' @importFrom stats optim
#' @import checkmate
#' @examples
#' a <- runif(100)
#' b <- runif(100)
#' form <- y ~ a + b
#' true_reg_coefficients <- c(2, 1)
#' mus <- exp(cbind(a, b) %*% matrix(true_reg_coefficients))
#' df <- data.frame(a = a, b = b, y = rchen_rpr(100, list(0.7, mus), 0.3))
#' reg_chen(df, form, tau = 0.3)
#'
reg_chen <- function(data, formula, tau = 0.5, stripped = F){ # For the reparametrized distribution only
  tryCatch(data <- as.data.frame(data),
           error = function(e) stop("The object provided as data is not coercible to data.frame"))
  checkmate::assert_data_frame(data, any.missing = F)
  checkmate::assert_formula(formula)
  checkmate::assert_number(tau, lower = 0, upper = 1)
  checkmate::assert_logical(stripped)

  #___________________________________ESTIMATION________________________________
  escore <- function(y, theta, X, tau) {
   lambda <- theta[1]
   beta <- theta[2:length(theta)]

   linear_predictor <- as.vector(X %*% as.matrix(beta))
   exponentiated_predictor <- exp(linear_predictor)

   beta_contribution <- as.vector(-(lambda * exponentiated_predictor^(lambda - 1) *
                                        exp(exponentiated_predictor^lambda) * (exp(exponentiated_predictor^lambda) +
                                        log(1 - tau) * exp(y^lambda) - log(1 - tau) - 1)) / ((exp(exponentiated_predictor^lambda) - 1)^2))

   lambda_contribution <- as.vector(((-log(1 - tau) * y^lambda * log(y) * exp(y^lambda) +
                                           (exponentiated_predictor^lambda) * log(exponentiated_predictor) * exp(exponentiated_predictor^lambda)) /
                                          (1 - exp(exponentiated_predictor^lambda))) + ((log(1 - tau) * (exponentiated_predictor^lambda) *
                                          log(exponentiated_predictor) * exp(exponentiated_predictor^lambda) * (1 - exp(y^lambda)))
                                          / ((1 - exp(exponentiated_predictor^lambda))^2)) + 1 / lambda + y^lambda * log(y) + log(y))

    tau_matrix <- diag(exp(linear_predictor))
    lambda_sum <- sum(lambda_contribution)
    beta_product <- t(X) %*% tau_matrix %*% beta_contribution

    result_vector <- c(lambda_sum, beta_product)
    return(result_vector)
  }

  log_likelihood_rpr_reg <- function(y, theta, X, tau){
    lambda <- theta[1]

    betas <- theta[2:length(theta)]
    mu_hats <- exp(X %*% as.matrix(betas))

    ll <- suppressWarnings(log(log(1 - tau) / (1 - exp(mu_hats^lambda))) +
                             log(lambda) + (lambda - 1) * log(y) +
                             (log(1 - tau) / (1 - exp(mu_hats^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
    return(sum(ll))
  }

  data <- as.data.frame(data)

  y <- as.vector(data[, all.vars(formula[[2]])])
  X <- as.matrix(data[, all.vars(formula[[3]])])
  # The gradient is suplied for the sake of optimization only,
  # if it was not supplied stats::optim would estimate it numerically
  # , so we can avoid the extra computation by calculating it manually

  # 0.7 starting point for lambda is arbitrary, just as the parameters for the regression
  initial_par <- c(0.7, rep(1, ncol(X)))

  estim <- stats::optim(par = initial_par,
                        fn = log_likelihood_rpr_reg,
                        hessian = T,
                        control = list(fnscale = -1, reltol = 1e-10), # Maybe test maxit arguments
                        gr = escore,
                        y = y,
                        X = X,
                        tau = tau)

  if(estim$convergence != 0){
    stop("The optimization did not converge!!!!
    The convergence value was: ", estim$convergence, ". Try looking at stats::optim documentation to see what this value means")
  }
  #____________________________BUILDING THE RETURN______________________________
  model <- list()
  model$names <- colnames(X)
  model$coefficients <- estim$par[2: length(estim$par)]
  model$formula <- formula
  model$lambda <- estim$par[1]
  model$tau <- tau

  model$call <-  match.call()
  return(model)
}

