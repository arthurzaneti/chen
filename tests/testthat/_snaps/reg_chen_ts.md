# output reg_arma

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = c(1, 2))
    Output
      Names:  beta0 phi1 phi2 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:  1.225 0.354 0.077 0.214 0.333 0.115 0.362 0.494 
      Case:  REG_ARMA 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 1, ma = c(1, 2))
    Output
      Names:  beta0 phi1 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:  1.219 0.422 0.143 0.370 0.132 0.370 0.493 
      Case:  REG_ARMA 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = 1)
    Output
      Names:  beta0 phi1 phi2 theta1 beta 1 beta 2 lambda 
      Coefficients:  0.930 0.137 0.448 0.372 0.108 0.368 0.489 
      Case:  REG_ARMA 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 2, ma = c(1, 2))
    Output
      Names:  beta0 phi2 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:  1.644 0.282 0.555 0.276 0.019 0.337 0.493 
      Case:  REG_ARMA 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = 2)
    Output
      Names:  beta0 phi1 phi2 theta2 beta 1 beta 2 lambda 
      Coefficients:   1.216  0.553 -0.158  0.460  0.169  0.387  0.492 
      Case:  REG_ARMA 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 2, ma = 2)
    Output
      Names:  beta0 phi2 theta2 beta 1 beta 2 lambda 
      Coefficients:  1.365 0.284 0.272 0.366 0.360 0.446 
      Case:  REG_ARMA 

# output reg_ar

    Code
      reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = c(1, 2))
    Output
      Names:  beta0 phi1 phi2 beta 1 beta 2 lambda 
      Coefficients:  1.278 0.346 0.170 0.432 0.173 0.514 
      Case:  REG_AR 

---

    Code
      reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 1)
    Output
      Names:  beta0 phi1 beta 1 beta 2 lambda 
      Coefficients:  1.482 0.447 0.418 0.147 0.508 
      Case:  REG_AR 

---

    Code
      reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 2)
    Output
      Names:  beta0 phi2 beta 1 beta 2 lambda 
      Coefficients:  1.575 0.342 0.462 0.237 0.498 
      Case:  REG_AR 

# output reg_ma

    Code
      reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = c(1, 2))
    Output
      Names:  beta0 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:   2.286  0.377  0.298  0.225 -0.531  0.526 
      Case:  REG_MA 

---

    Code
      reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = 1)
    Output
      Names:  beta0 theta1 beta 1 beta 2 lambda 
      Coefficients:   1.974  0.296  0.231 -0.442  0.499 
      Case:  REG_MA 

---

    Code
      reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = 2)
    Output
      Names:  beta0 theta2 beta 1 beta 2 lambda 
      Coefficients:   2.056  0.230  0.359 -0.520  0.467 
      Case:  REG_MA 

