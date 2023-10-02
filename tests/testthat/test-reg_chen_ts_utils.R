# Preping for lls testing
y <- df
y_cut <-y[-c(99, 100), ]
log_y <- log(y)
cvar <- cbind(df$cvar1, df$cvar2)
n <- 100
n_ar <- n_ma <- max_ar <- max_ma <- max_arma <- 2
ar <- ma <- c(1, 2)
tau <- 0.5


test_that("lls_reg",{
#y, y_cut, log_y, vars, n, n_ar, n_ma, ar, ma, max_arma, cvar, tau
  expect_snapshot(ll_REG_ARMA(y$arma, y_cut$arma, log_y$arma,
                          vars = c(beta0 = 1, phi = c(0.3, 0.2), theta = c(0.4, 0.2), lambda = 0.5),
                          n, n_ar, n_ma, ar, ma, max_arma, cvar, tau))

  expect_snapshot(ll_REG_AR(y$ar, y_cut$ar, log_y$ar,
                        vars = c(beta0 = 1, phi = c(0.3, 0.2), lambda = 0.5),
                        n, n_ar, ar, max_ar, cvar, tau))

  expect_snapshot(ll_REG_MA(y$ma, y_cut$ma, log_y$ma,
                        vars = c(beta0 = 1, theta = c(0.4, 0.2), lambda = 0.5),
                        n, n_ma, ma, max_ma, cvar, tau))
})
