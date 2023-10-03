test_that("output", {
  expect_snapshot(ll_chen_rpr(r_values_rpr, c(0.7, 7), 0.5))
  expect_snapshot(ll_chen_rpr(r_values_rpr, c(0.1, 4), 0.9))
  expect_snapshot(ll_chen_rpr(r_values_rpr, c(1.4, 3), 0.4))
  expect_snapshot(ll_chen_rpr(r_values_rpr, c(4, 0.3), 0.5))
  expect_snapshot(ll_chen_rpr(r_values_rpr, c(2, 3.4), 0.1))
})
