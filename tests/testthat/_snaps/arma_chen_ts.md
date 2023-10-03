# output arma

    Code
      arma_chen_ts(df$arma, ar = 1:2, ma = 1:2)
    Output
      Names:  intercept phi1 phi2 theta1 theta2 lambda 
      Coefficients:  0.879 0.160 0.317 0.386 0.279 0.516 
      Case:  ARMA 

---

    Code
      arma_chen_ts(df$arma, ar = 1, ma = 1:2)
    Output
      Names:  intercept phi1 theta1 theta2 lambda 
      Coefficients:  0.862 0.477 0.082 0.366 0.512 
      Case:  ARMA 

---

    Code
      arma_chen_ts(df$arma, ar = 1:2, ma = 1)
    Output
      Names:  intercept phi1 phi2 theta1 lambda 
      Coefficients:  0.756 0.045 0.537 0.468 0.501 
      Case:  ARMA 

---

    Code
      arma_chen_ts(df$arma, ar = 2, ma = 1:2)
    Output
      Names:  intercept phi2 theta1 theta2 lambda 
      Coefficients:  1.009 0.387 0.522 0.266 0.513 
      Case:  ARMA 

---

    Code
      arma_chen_ts(df$arma, ar = 1:2, ma = 2)
    Output
      Names:  intercept phi1 phi2 theta2 lambda 
      Coefficients:  0.725 0.527 0.044 0.315 0.510 
      Case:  ARMA 

---

    Code
      arma_chen_ts(df$arma, ar = 2, ma = 2)
    Output
      Names:  intercept phi2 theta2 lambda 
      Coefficients:  0.866 0.406 0.262 0.444 
      Case:  ARMA 

# output ar

    Code
      arma_chen_ts(df$ar, ar = 1:2)
    Output
      Names:  intercept phi1 phi2 lambda 
      Coefficients:  0.963 0.248 0.284 0.526 
      Case:  AR 

---

    Code
      arma_chen_ts(df$ar, ar = 1)
    Output
      Names:  intercept phi1 lambda 
      Coefficients:  1.177 0.386 0.503 
      Case:  AR 

---

    Code
      arma_chen_ts(df$ar, ar = 2)
    Output
      Names:  intercept phi2 lambda 
      Coefficients:  1.138 0.407 0.508 
      Case:  AR 

# output ma

    Code
      arma_chen_ts(df$ma, ma = 1:2)
    Output
      Names:  intercept theta1 theta2 lambda 
      Coefficients:  1.265 0.259 0.227 0.509 
      Case:  MA 

---

    Code
      arma_chen_ts(df$ma, ma = 1)
    Output
      Names:  intercept theta1 lambda 
      Coefficients:  1.190 0.206 0.490 
      Case:  MA 

---

    Code
      arma_chen_ts(df$ma, ma = 2)
    Output
      Names:  intercept theta2 lambda 
      Coefficients:  1.144 0.153 0.472 
      Case:  MA 

