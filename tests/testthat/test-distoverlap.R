test_that("input must be a draws_matrix", {
  x <- matrix(rnorm(40), ncol = 4)
  y <- x
  class(y) <- c("draws_matrix", "draws", "matrix")
  expect_error(calc_drawsoverlap(x))
  expect_error(calc_drawsoverlap(y), NA)
})

test_that("diag elements are 1", {
  n_col <- 4
  x <- matrix(rnorm(n_col * 10), ncol = n_col)
  class(x) <- c("draws_matrix", "draws", "matrix")
  expect_equal(diag(calc_drawsoverlap(x)), rep(1, n_col))
})
