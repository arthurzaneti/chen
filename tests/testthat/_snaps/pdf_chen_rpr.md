# pdf_chen_rpr works correctly

    Code
      cat(pdf_chen_rpr(1, c(0.2, 2), 0.2))
    Output
      0.04713487

---

    Code
      cat(pdf_chen_rpr(2, c(0.3, 3), 0.3))
    Output
      0.05614649

---

    Code
      cat(pdf_chen_rpr(3, c(1, 10), 0.2))
    Output
      0.0002034505

---

    Code
      cat(pdf_chen_rpr(4, c(1, 1)))
    Output
      8.972594e-09

---

    Code
      cat(pdf_chen_rpr(5, c(0.6789, 7.89423), 0.7))
    Output
      0.113213

---

    Code
      cat(pdf_chen_rpr(6, c(2, 9), 0.8))
    Output
      5.528455e-19

