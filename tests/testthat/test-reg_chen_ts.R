test_that("basic output", {
  expect_snapshot(reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = c(1, 2), ma = c(1, 2)))
  expect_snapshot(reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = c(1, 2)))
  expect_snapshot(reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = c(1, 2)))
  expect_snapshot(reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 1, ma = 1))
  expect_snapshot(reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 1))
  expect_snapshot(reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ma = 1))
})

test_that("edge cases",{
  expect_snapshot(reg_chen_ts(df, reg_arma ~ cvar1 + cvar2, ar = 2, ma = 2))
  expect_snapshot(reg_chen_ts(df, reg_ar ~ cvar1 + cvar2, ar = 2))
  expect_snapshot(reg_chen_ts(df, reg_ma ~ cvar1 + cvar2, ma = 2))
})
