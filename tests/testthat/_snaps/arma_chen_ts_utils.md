# reg_arma_case

    Code
      reg_arma_case(T, T, T)
    Output
      [1] "REG_ARMA"

---

    Code
      reg_arma_case(T, T, F)
    Output
      [1] "ARMA"

---

    Code
      reg_arma_case(T, F, F)
    Output
      [1] "AR"

---

    Code
      reg_arma_case(F, T, T)
    Output
      [1] "REG_MA"

---

    Code
      reg_arma_case(F, T, F)
    Output
      [1] "MA"

---

    Code
      reg_arma_case(T, F, T)
    Output
      [1] "REG_AR"

# lls

    Code
      ll_ARMA(y$arma, y_cut$arma, log_y$arma, vars = c(beta0 = 1, phi = c(0.3, 0.2),
      theta = c(0.4, 0.2), lambda = 0.5), n, n_ar, n_ma, ar, ma, max_arma, tau)
    Output
      [1] -246.6347

---

    Code
      ll_AR(y$ar, y_cut$ar, log_y$ar, vars = c(beta0 = 1, phi = c(0.3, 0.2), lambda = 0.5),
      n, n_ar, ar, max_ar, tau)
    Output
      [1] -248.3657

---

    Code
      ll_MA(y$ma, y_cut$ma, log_y$ma, vars = c(beta0 = 1, theta = c(0.4, 0.2),
      lambda = 0.5), n, n_ma, ma, max_ma, tau)
    Output
      [1] -214.8399

