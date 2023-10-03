test_that("output", {
  expect_snapshot(ll_chen(r_values, c(0.7, 0.1)))
  expect_snapshot(ll_chen(r_values, c(0.1, 0.3)))
  expect_snapshot(ll_chen(r_values, c(1.4, 0.2)))
  expect_snapshot(ll_chen(r_values, c(4, 0.01)))
  expect_snapshot(ll_chen(r_values, c(2, 0.001)))
})
