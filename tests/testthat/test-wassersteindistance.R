test_that("input must be numeric", {
  x <- letters[1:4]
  y <- 1:4
  expect_error(calc_wasserstein(x, y))
  expect_error(calc_wasserstein(y, x))
  expect_error(calc_wasserstein(y, y), NA)
})

test_that("input must be a draws_matrix", {
  x <- matrix(rnorm(40), ncol = 4)
  y <- x
  class(y) <- c("draws_matrix", "draws", "matrix")
  expect_error(calc_drawswasserstein(x))
  expect_error(calc_drawswasserstein(y), NA)
})
