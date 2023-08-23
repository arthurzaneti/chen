testthat::test_that("pdf_chen works correctly", {
  y <- c(1, 2, 3)
  theta <- c(0.5, 0.1)

  result <- pdf_chen(y, theta)
  expected <- c(0.1144565, 0.1065206, 0.1024674)
  expect_equal(result, expected, tolerance = 1e-6)
})
