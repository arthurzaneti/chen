#' Title
#'
#' @param data
#' @param formula
#' @param tau
#'
#' @return
#' @export
#'
#' @examples
reg_chen <- function(data, formula, tau = 0.5){ # For the reparametrized distribution only
# stopifnot(is.numeric(y))
# stopifnot(length(y) == 1)
# stopifnot(is.matrix(cvar) || is.vector(cvar))
# if(is.matrix(cvar)){
#   stopifnot(nrow(cvar) == length(y))
# }else{
#   stopifnot(length(cvar) == length(y))
# }
# stopifnot(is.numeric(tau))
# stopifnot(length(tau) == 1)
  #_____________________________________________________________________________
  # escore_func <- function(y, theta, X, tau) {
  #   lambda <- theta[1]
  #   beta <- theta[2:length(theta)]
  #
  #   linear_predictor <- X %*% as.matrix(beta)
  #   exponentiated_predictor <- exp(linear_predictor)
  #
  #   beta_contribution <- as.vector(-(lambda * exponentiated_predictor^(lambda - 1) *
  #                                      exp(exponentiated_predictor^lambda) * (exp(exponentiated_predictor^lambda) +
  #                                      log(1 - tau) * exp(y^lambda) - log(1 - tau) - 1)) / ((exp(exponentiated_predictor^lambda) - 1)^2))
  #
  #   lambda_contribution <- as.vector(((-log(1 - tau) * y^lambda * log(y) * exp(y^lambda) +
  #                                        (exponentiated_predictor^lambda) * log(exponentiated_predictor) * exp(exponentiated_predictor^lambda)) /
  #                                       (1 - exp(exponentiated_predictor^lambda))) + ((log(1 - tau) * (exponentiated_predictor^lambda) *
  #                                       log(exponentiated_predictor) * exp(exponentiated_predictor^lambda) * (1 - exp(y^lambda)))
  #                                       / ((1 - exp(exponentiated_predictor^lambda))^2)) + 1 / lambda + y^lambda * log(y) + log(y))
  #   tau_matrix <- diag(exp(linear_predictor))
  #
  #   lambda_sum <- sum(lambda_contribution)
  #   beta_product <- t(X) %*% tau_matrix %*% beta_contribution
  #
  #   result_vector <- c(lambda_sum, beta_product)
  #   return(result_vector)
  # }
  log_likelihood_rpr_reg <- function(y, theta, X, tau){
    lambda <- theta[1]

    beta <- theta[2:length(theta)]

    mu_hats <- exp(X %*% as.matrix(beta))

    ll <- suppressWarnings(log(log(1 - tau) / (1 - exp(mu_hats^lambda))) +
                             log(lambda) + (lambda - 1) * log(y) +
                             (log(1 - tau) / (1 - exp(mu_hats^lambda))) * (1 - exp(y^lambda)) + (y^lambda))
    return(sum(ll))
  }


  data <- as.data.frame(data)

  # Reordering the columns such that response variable is the first one
  data <- data[, c(
    which(colnames(data) == formula[[2]]),
    which(colnames(data) != formula[[2]])
  )]

  y <- unlist(data[, 1]) # Else the y variable would be a data.frame
  X <- model.matrix(formula, data) # Simply adds the intercept and removes the response variable

  betas <- lm(formula = formula, data = data)$coefficients

  # The gradient is suplied for the sake of optimization only,
  # if it was not supplied optim would estimate it numerically
  # , so we can avoid the extra computation by calculating it manually
  estim <- stats::optim(par = c(0.7, betas),
                        fn = log_likelihood_rpr_reg,
                        hessian = T,
                        control = list(fnscale = -1, reltol = 1e-10), # Maybe test maxit arguments
                        y = y,
                        X = X,
                        tau = tau)


  reg_model <- estim
  class(reg_model) <- "reg_chen_model"
  return(reg_model)
}

