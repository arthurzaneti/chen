# output

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.107  0.247  0.433 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.143 -0.364  0.564 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.337  1.911 -0.412 

---

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.107  0.247  0.433 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.143 -0.364  0.564 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.337  1.911 -0.412 

# output with bootstrap

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.167  0.264  0.458 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.160  1.782  0.074 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -2.941  1.748  0.711 

---

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.173  0.219  0.456 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.346  2.053  0.097 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -2.767  1.740  0.640 

