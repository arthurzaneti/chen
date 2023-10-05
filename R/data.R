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

  CHARMA_arma <- arma_chen_ts(df$arma, ar = 1:2, ma = 1:2)
  CHARMA_ar <- arma_chen_ts(df$ar, ar = 1:2)
  CHARMA_ma <- arma_chen_ts(df$ma, ma = 1:2)

  reg_CHARMA_arma <- reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = c(1, 2))
  reg_CHARMA_ar <- reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = c(1, 2))
  reg_CHARMA_ma <- reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = c(1, 2))

  models <- list(CHARMA_arma, CHARMA_ar, CHARMA_ma,
                          reg_CHARMA_arma, reg_CHARMA_ar, reg_CHARMA_ma)

  usethis::use_data(df, models, r_values, r_values_rpr, internal = T)
}
