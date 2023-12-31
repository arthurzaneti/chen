test_that("output with seed", {
  cvar <-  cbind(df$cvar1, df$cvar2)
  set.seed(69)
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4, 0.2), ma_coef = c(0.2, 0.1), reg_coef = c(0.6, -0.1), cvar = cvar))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4), ma_coef = c(0.2), reg_coef = c(0.6, 0), cvar = cvar))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4, 0.2), reg_coef = c(0.6, -0.1), cvar = cvar))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4), reg_coef = c(0.6, 0), cvar = cvar))
  expect_snapshot(rchen_ts(100, 1, 0.3, ma_coef = c(0.2, 0.1), reg_coef = c(0.6, -0.1), cvar = cvar))
  expect_snapshot(rchen_ts(100, 1, 0.3, ma_coef = c(0.2), reg_coef = c(0.6, 0), cvar = cvar))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4, 0.2), ma_coef = c(0.2, 0.1)))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4), ma_coef = c(0.2)))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4, 0.2)))
  expect_snapshot(rchen_ts(100, 1, 0.3, ar_coef = c(0.4)))
  expect_snapshot(rchen_ts(100, 1, 0.3, ma_coef = c(0.2, 0.1)))
  expect_snapshot(rchen_ts(100, 1, 0.3, ma_coef = c(0.2)))
})
