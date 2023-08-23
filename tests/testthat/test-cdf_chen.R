testthat::test_that("cdf_chen works correctly", {
  y <- c(1, 2, 3)
  theta <- c(0.5, 0.1)

  result <- cdf_chen(y, theta)
  expected <- c(0.1578761, 0.2675242, 0.3720052)
  expect_equal(result, expected, tolerance = 1e-6)
})
