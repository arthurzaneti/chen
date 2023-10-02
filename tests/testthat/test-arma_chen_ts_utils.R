test_that("reg_arma_case", {
  # Function is for reg_arma_ts and arma_chen_ts
  expect_snapshot(reg_arma_case(T, T, T))
  expect_snapshot(reg_arma_case(T, T, F))
  expect_snapshot(reg_arma_case(T, F, F))
  expect_snapshot(reg_arma_case(F, T, T))
  expect_snapshot(reg_arma_case(F, T, F))
  expect_snapshot(reg_arma_case(T, F, T))
})

# Preping for lls testing
y <- df
y_cut <-y[-c(99, 100), ]
log_y <- log(y)
n <- 100
n_ar <- n_ma <- max_ar <- max_ma <- max_arma <- 2
ar <- ma <- c(1, 2)
tau <- 0.5

test_that("lls",{

  expect_snapshot(ll_ARMA(y$arma, y_cut$arma, log_y$arma,
                          vars = c(beta0 = 1, phi = c(0.3, 0.2), theta = c(0.4, 0.2), lambda = 0.5),
                          n, n_ar, n_ma, ar, ma, max_arma, tau))

  expect_snapshot(ll_AR(y$ar, y_cut$ar, log_y$ar,
                          vars = c(beta0 = 1, phi = c(0.3, 0.2), lambda = 0.5),
                          n, n_ar, ar, max_ar, tau))

  expect_snapshot(ll_MA(y$ma, y_cut$ma, log_y$ma,
                          vars = c(beta0 = 1, theta = c(0.4, 0.2), lambda = 0.5),
                          n, n_ma, ma, max_ma, tau))
})

