test_that("output with seed", {
  set.seed(69)
  expect_snapshot(rchen_rpr(100, c(0.4, 4)))
  expect_snapshot(rchen_rpr(100, c(0.7, 7), 0.3))
  expect_snapshot(rchen_rpr(100, c(1, 1), 0.1))
  expect_snapshot(rchen_rpr(100, c(2, 0.4), 0.7))
})
