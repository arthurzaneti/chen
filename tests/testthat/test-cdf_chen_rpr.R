testthat::test_that("cdf_chen_rpr works correctly", {
  y <- c(1, 2, 3)
  theta <- c(0.7, 7)
  tau <- 0.3

  result <- cdf_chen_rpr(y, theta, tau)
  expected <- c(0.01252444, 0.02945413, 0.05457379)
  expect_equal(result, expected, tolerance = 1e-6)
})
