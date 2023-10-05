# lls_reg

    Code
      ll_REG_ARMA(y$arma, y_cut$arma, log_y$arma, vars = c(beta0 = 1, phi = c(0.3,
        0.2), theta = c(0.4, 0.2), lambda = 0.5), n, n_ar, n_ma, ar, ma, max_arma,
      cvar, tau)
    Output
      [1] -253.5783

---

    Code
      ll_REG_AR(y$ar, y_cut$ar, log_y$ar, vars = c(beta0 = 1, phi = c(0.3, 0.2),
      lambda = 0.5), n, n_ar, ar, max_ar, cvar, tau)
    Output
      [1] -270.418

---

    Code
      ll_REG_MA(y$ma, y_cut$ma, log_y$ma, vars = c(beta0 = 1, theta = c(0.4, 0.2),
      lambda = 0.5), n, n_ma, ma, max_ma, cvar, tau)
    Output
      [1] -226.4515

