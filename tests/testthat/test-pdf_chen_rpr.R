testthat::test_that("pdf_chen_rpr works correctly", {
  y <- c(1, 2, 3)
  theta <- c(0.7, 7)
  tau <- 0.3

  result <- pdf_chen_rpr(y, theta, tau)
  expected <- c(0.01378214, 0.02054549, 0.03020307)
  expect_equal(result, expected, tolerance = 1e-6)
})
