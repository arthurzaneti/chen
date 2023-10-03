test_that("output arma", {
  expect_snapshot(arma_chen_ts(df$arma, ar = 1:2, ma = 1:2))
  expect_snapshot(arma_chen_ts(df$arma, ar = 1, ma = 1:2))
  expect_snapshot(arma_chen_ts(df$arma, ar = 1:2, ma = 1))
  expect_snapshot(arma_chen_ts(df$arma, ar = 2, ma = 1:2))
  expect_snapshot(arma_chen_ts(df$arma, ar = 1:2, ma = 2))
  expect_snapshot(arma_chen_ts(df$arma, ar = 2, ma = 2))
})

test_that("output ar", {
  expect_snapshot(arma_chen_ts(df$ar, ar = 1:2))
  expect_snapshot(arma_chen_ts(df$ar, ar = 1))
  expect_snapshot(arma_chen_ts(df$ar, ar = 2))
})

test_that("output ma", {
  expect_snapshot(arma_chen_ts(df$ma, ma = 1:2))
  expect_snapshot(arma_chen_ts(df$ma, ma = 1))
  expect_snapshot(arma_chen_ts(df$ma, ma = 2))
})
