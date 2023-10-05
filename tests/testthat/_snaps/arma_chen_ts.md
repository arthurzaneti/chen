# output arma

    Code
      arma_chen_ts(df$arma, ar = 1:2, ma = 1:2)
    Output
      Names:  beta0 phi1 phi2 theta1 theta2 lambda 
      Coefficients:  0.935 0.416 0.098 0.179 0.316 0.525 
      Lambda:  0.5248963 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$arma, ar = 1, ma = 1:2)
    Output
      Names:  beta0 phi1 theta1 theta2 lambda 
      Coefficients:  0.926 0.512 0.093 0.347 0.524 
      Lambda:  0.5242294 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$arma, ar = 1:2, ma = 1)
    Output
      Names:  beta0 phi1 phi2 theta1 lambda 
      Coefficients:   1.112 -0.120  0.572  0.726  0.524 
      Lambda:  0.5237975 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$arma, ar = 2, ma = 1:2)
    Output
      Names:  beta0 phi2 theta1 theta2 lambda 
      Coefficients:  1.070 0.466 0.633 0.080 0.523 
      Lambda:  0.5226455 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$arma, ar = 1:2, ma = 2)
    Output
      Names:  beta0 phi1 phi2 theta2 lambda 
      Coefficients:   0.826  0.589 -0.020  0.310  0.524 
      Lambda:  0.5241569 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$arma, ar = 2, ma = 2)
    Output
      Names:  beta0 phi2 theta2 lambda 
      Coefficients:  1.310 0.251 0.369 0.470 
      Lambda:  0.4700955 
      Tau:  0.5 

# output ar

    Code
      arma_chen_ts(df$ar, ar = 1:2)
    Output
      Names:  beta0 phi1 phi2 lambda 
      Coefficients:  1.016 0.235 0.262 0.518 
      Lambda:  0.5184981 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$ar, ar = 1)
    Output
      Names:  beta0 phi1 lambda 
      Coefficients:  1.305 0.307 0.503 
      Lambda:  0.5026407 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$ar, ar = 2)
    Output
      Names:  beta0 phi2 lambda 
      Coefficients:  1.237 0.346 0.499 
      Lambda:  0.4988895 
      Tau:  0.5 

# output ma

    Code
      arma_chen_ts(df$ma, ma = 1:2)
    Output
      Names:  beta0 theta1 theta2 lambda 
      Coefficients:  1.140 0.271 0.076 0.562 
      Lambda:  0.5616795 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$ma, ma = 1)
    Output
      Names:  beta0 theta1 lambda 
      Coefficients:  1.120 0.250 0.561 
      Lambda:  0.5613471 
      Tau:  0.5 

---

    Code
      arma_chen_ts(df$ma, ma = 2)
    Output
      Names:  beta0 theta2 lambda 
      Coefficients:  1.072 0.103 0.544 
      Lambda:  0.5437207 
      Tau:  0.5 

