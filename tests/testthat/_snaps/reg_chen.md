# output

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  0.032 0.573 0.308 
      Lambda:  0.743066 
      Tau:  0.5 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  0.277 0.251 0.272 
      Lambda:  0.3221562 
      Tau:  0.5 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -2.522  1.416  0.571 
      Lambda:  0.5845893 
      Tau:  0.5 

---

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  0.032 0.573 0.308 
      Lambda:  0.743066 
      Tau:  0.5 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  0.277 0.251 0.272 
      Lambda:  0.3221562 
      Tau:  0.5 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -2.522  1.416  0.571 
      Lambda:  0.5845893 
      Tau:  0.5 

# output with bootstrap

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.009  0.583  0.327 
      Lambda:  0.7563326 
      Tau:  0.5 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.222 -1.816  0.837 
      Lambda:  0.3043031 
      Tau:  0.5 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -1.774  1.765  0.223 
      Lambda:  0.5276884 
      Tau:  0.5 

---

    Code
      reg_chen(df_test1, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -0.070  0.579  0.339 
      Lambda:  0.7557841 
      Tau:  0.5 

---

    Code
      reg_chen(df_test2, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:   0.525 -1.522  0.468 
      Lambda:  0.3055165 
      Tau:  0.5 

---

    Code
      reg_chen(df_test3, y ~ cvar1 + cvar2, n_bootstrap = 30)
    Output
      Names:  beta 0 beta 1 beta 2 
      Coefficients:  -1.524  1.786  0.119 
      Lambda:  0.5266759 
      Tau:  0.5 

