test_that("output",{
  expect_snapshot(estim_chen(r_values))
  expect_snapshot(estim_chen(r_values, clvl = 0.95))

  # Kind of funky but was the way I found to test something even though boot is random
  test_boot <- estim_chen(r_values, clvl = 0.95, n_bootstrap = 100)
  expect_true(all((test_boot$t0 - test_boot$t) < .1))
})
