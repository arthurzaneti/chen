set.seed(1)
rvalues <- rchen(100, c(0.7, 7, 0.5))

test_that("Return types",{
  expect_vector(estim_chen_rpr(rvalues, tau = 0.5))
  expect_true(is.matrix(estim_chen_rpr(rvalues, tau = 0.5, clvl = 0.95)))
  expect_list(estim_chen_rpr(rvalues, tau = 0.5, full = T), len = 6)
  expect_list(estim_chen_rpr(rvalues, tau = 0.5, full = T, clvl = 0.95), len = 7)
})

test_that("Calculations", {
  # Não sei por enquanto
})

