test_that("Invalid inputs",{
  expect_identical(rchen(1, 1), NA_real_)
  expect_warning(rchen(c(0.7, -1), 10))
  expect_error(rchen(c(0.5, 0.5), "a"))
  expect_error(rchen(c(0.5, "a"), 1))
  expect_error(rchen(c("a", 0.5), 1))
})


