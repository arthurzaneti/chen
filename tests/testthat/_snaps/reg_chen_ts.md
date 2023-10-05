# output reg_arma

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = c(1, 2))
    Output
      Names:  beta0 phi1 phi2 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:   2.613  0.006  0.169  0.529  0.164  0.368 -0.057  0.527 
      Lambda:  0.5273457 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 1, ma = c(1, 2))
    Output
      Names:  beta0 phi1 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:   2.512  0.203  0.335  0.218  0.383 -0.060  0.527 
      Lambda:  0.5268719 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = 1)
    Output
      Names:  beta0 phi1 phi2 theta1 beta 1 beta 2 lambda 
      Coefficients:   2.343 -0.104  0.367  0.613  0.344 -0.049  0.526 
      Lambda:  0.5260258 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 2, ma = c(1, 2))
    Output
      Names:  beta0 phi2 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:   2.623  0.172  0.536  0.163  0.368 -0.058  0.527 
      Lambda:  0.5273581 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = 2)
    Output
      Names:  beta0 phi1 phi2 theta2 beta 1 beta 2 lambda 
      Coefficients:   1.661  0.525 -0.055  0.104  0.376 -0.044  0.525 
      Lambda:  0.5254426 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 2, ma = 2)
    Output
      Names:  beta0 phi2 theta2 beta 1 beta 2 lambda 
      Coefficients:  1.921 0.293 0.005 0.345 0.102 0.480 
      Lambda:  0.4799817 
      Tau:  0.5 

# output reg_ar

    Code
      reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = c(1, 2))
    Output
      Names:  beta0 phi1 phi2 beta 1 beta 2 lambda 
      Coefficients:   1.738  0.316  0.110  0.445 -0.036  0.486 
      Lambda:  0.4863323 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 1)
    Output
      Names:  beta0 phi1 beta 1 beta 2 lambda 
      Coefficients:   1.883  0.361  0.396 -0.003  0.485 
      Lambda:  0.4851784 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 2)
    Output
      Names:  beta0 phi2 beta 1 beta 2 lambda 
      Coefficients:   2.260  0.232  0.474 -0.042  0.462 
      Lambda:  0.4618147 
      Tau:  0.5 

# output reg_ma

    Code
      reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = c(1, 2))
    Output
      Names:  beta0 theta1 theta2 beta 1 beta 2 lambda 
      Coefficients:   1.630  0.321  0.103  0.612 -0.445  0.521 
      Lambda:  0.5211248 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = 1)
    Output
      Names:  beta0 theta1 beta 1 beta 2 lambda 
      Coefficients:   1.761  0.293  0.635 -0.509  0.521 
      Lambda:  0.5208398 
      Tau:  0.5 

---

    Code
      reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = 2)
    Output
      Names:  beta0 theta2 beta 1 beta 2 lambda 
      Coefficients:   0.894  0.088  0.576 -0.173  0.495 
      Lambda:  0.4945544 
      Tau:  0.5 

