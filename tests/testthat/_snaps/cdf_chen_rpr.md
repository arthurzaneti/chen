# cdf_chen_rpr works correctly

    Code
      cat(cdf_chen_rpr(1, c(0.2, 2), 0.2))
    Output
      0.1630562

---

    Code
      cat(cdf_chen_rpr(2, c(0.3, 3), 0.3))
    Output
      0.2493088

---

    Code
      cat(cdf_chen_rpr(3, c(1, 10), 0.2))
    Output
      0.00019334

---

    Code
      cat(cdf_chen_rpr(4, c(1, 1)))
    Output
      1

---

    Code
      cat(cdf_chen_rpr(5, c(0.6789, 7.89423), 0.7))
    Output
      0.325199

---

    Code
      cat(cdf_chen_rpr(6, c(2, 9), 0.8))
    Output
      0

