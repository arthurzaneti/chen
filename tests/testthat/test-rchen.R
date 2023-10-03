test_that("output with seed", {
  set.seed(69)
  expect_snapshot(rchen(100, c(0.4, 0.4)))
  expect_snapshot(rchen(100, c(0.7, 0.7)))
  expect_snapshot(rchen(100, c(1, 0.1)))
  expect_snapshot(rchen(100, c(2, 4)))
})
