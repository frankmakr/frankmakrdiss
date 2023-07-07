test_that("vec_x must be numeric", {
  x <- letters[1:4]
  y <- 1:4
  expect_error(calc_hdr(x))
  expect_error(calc_hdr(y), NA)
})

test_that("0 < probs < 1", {
  x <- rnorm(4)
  probs_a <- letters[1]
  probs_b <- -1:3
  probs_c <- seq(0.1, 0.9, 0.1)
  expect_error(calc_hdr(x, probs_a))
  expect_error(calc_hdr(x, probs_b))
  expect_error(calc_hdr(x, probs_c), NA)
})
