test_that("input must be numeric", {
  x <- letters[1:4]
  expect_error(tabulate_paic(x))
})

test_that("diag elements are 0.5", {
  x <- rnorm(4)
  expect_equal(diag(tabulate_paic(x)$p_delta_aic), rep(0.5, length(x)))
})
