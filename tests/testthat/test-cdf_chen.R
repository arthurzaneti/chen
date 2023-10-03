testthat::test_that("cdf_chen works correctly", {
  expect_snapshot(cdf_chen(1, c(0.2, 0.01)))
  expect_snapshot(cdf_chen(2, c(0.3, 0.1)))
  expect_snapshot(cdf_chen(3, c(1, 0.2)))
  expect_snapshot(cdf_chen(4, c(1, 0.23)))
  expect_snapshot(cdf_chen(5, c(0.6789, 0.89423)))
  expect_snapshot(cdf_chen(6, c(2, 2)))
})
