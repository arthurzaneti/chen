generate_data <- function(){
  cvar1 <- runif(100)
  cvar2 <- runif(100, 2, 3)
  cvar <- cbind(cvar1, cvar2)

  reg_arma <- rchen_ts(100, 1, 0.5, ar_coef = c(0.3, 0.2), ma_coef = c(0.3, 0.2), reg_coef = c(0.3, 0.2), cvar = cvar)
  reg_ar <- rchen_ts(100, 1, 0.5, ar_coef = c(0.3, 0.2), reg_coef = c(0.3, 0.2), cvar = cvar)
  reg_ma <- rchen_ts(100, 1, 0.5, ma_coef = c(0.3, 0.2), reg_coef = c(0.3, 0.2), cvar = cvar)
  arma <- rchen_ts(100, 1, 0.5, ar_coef = c(0.3, 0.2), ma_coef = c(0.3, 0.2))
  ar <- rchen_ts(100, 1, 0.5, ar_coef = c(0.3, 0.2))
  ma <- rchen_ts(100, 1, 0.5, ma_coef = c(0.3, 0.2))

  df <- data.frame(cvar1 = cvar1,
                   cvar2 = cvar2,
                   reg_arma = reg_arma,
                   reg_ar = reg_ar,
                   reg_ma = reg_ma,
                   arma = arma,
                   ar = ar,
                   ma = ma)

  r_values <- rchen(100, c(1, 0.2))
  r_values_rpr <- rchen_rpr(100, c(0.7, 7))
  usethis::use_data(df, r_values, r_values_rpr, internal = T)
}
