set.seed(1)
rvalues <- rchen_rpr(100, c(0.7, 7), 0.5)

test_that("Return length",{
  expect_list(estim_chen_rpr(rvalues), len = 1)
  expect_list(estim_chen_rpr(rvalues, clvl = 0.95), len = 2)
  expect_list(estim_chen_rpr(rvalues, clvl = 0.95, n_bootstrap = 10), len = 4)
})

