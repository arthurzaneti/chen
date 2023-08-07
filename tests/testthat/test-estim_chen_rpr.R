set.seed(1)
rvalues <- rchen(100, c(0.7, 7, 0.5))

test_that("Return types",{
  expect_vector(estim_chen_rpr(rvalues, tau = 0.5))
  expect_true(is.matrix(estim_chen_rpr(rvalues, tau = 0.5, ci_alpha = 0.05)))
  expect_list(estim_chen_rpr(rvalues, tau = 0.5, full = T), len = 6)
  expect_list(estim_chen_rpr(rvalues, tau = 0.5, full = T, ci_alpha = 0.05), len = 7)
})

test_that("Calculations", {
  # Não sei por enquanto
})

