set.seed(1)
rvalues <- rchen(100, c(1,1))

test_that("Return types",{
  expect_vector(estim_chen(rvalues))
  expect_true(is.matrix(estim_chen(rvalues, clvl = 0.95)))
  expect_list(estim_chen(rvalues, full = T), len = 6)
  expect_list(estim_chen(rvalues, full = T, clvl = 0.95), len = 7)
})

test_that("Calculations", {
  # Não sei por enquanto
})
