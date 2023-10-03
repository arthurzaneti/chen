testthat::test_that("cdf_chen_rpr works correctly", {
  expect_snapshot(cdf_chen_rpr(1, c(0.2, 2), 0.2))
  expect_snapshot(cdf_chen_rpr(2, c(0.3, 3), 0.3))
  expect_snapshot(cdf_chen_rpr(3, c(1, 10), 0.2))
  expect_snapshot(cdf_chen_rpr(4, c(1, 1)))
  expect_snapshot(cdf_chen_rpr(5, c(0.6789, 7.89423), 0.7))
  expect_snapshot(cdf_chen_rpr(6, c(2, 9), 0.8))
})
