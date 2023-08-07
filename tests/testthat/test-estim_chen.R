set.seed(1)
rvalues <- rchen(100, c(1,1))

test_that("Return types",{
  expect_vector(estim_chen(rvalues))
  expect_true(is.matrix(estim_chen(rvalues, ci_alpha = 0.05)))
  expect_list(estim_chen(rvalues, full = T), len = 6)
  expect_list(estim_chen(rvalues, full = T, ci_alpha = 0.05), len = 7)
})

test_that("Calculations", {
  # Não sei por enquanto
})
