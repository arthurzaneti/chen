set.seed(1)
rvalues <- rchen(100, c(1,1))

test_that("Return length",{
  expect_list(estim_chen(rvalues), len = 1)
  expect_list(estim_chen(rvalues, clvl = 0.95), len = 2)
  expect_list(estim_chen(rvalues, clvl = 0.95, n_bootstrap = 10), len = 4)
})
