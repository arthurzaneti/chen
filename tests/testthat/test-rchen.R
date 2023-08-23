test_that("Invalid inputs",{
  expect_error(rchen(1, 1))
  expect_error(rchen(10, c(0.7, -1)))
  expect_error(rchen(1, c(0.5, "a")))
  expect_error(rchen(1, c("a", 0.5)))
})

# Really hard to test random functions, will do it latter


