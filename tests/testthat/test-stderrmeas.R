test_that("data must be numeric", {
  x <- letters[1:4]
  y <- 1:4
  sd_x <- 1
  rel <- 0.9
  expect_error(calc_semci(x, sd_x, rel))
  expect_error(calc_semci(y, sd_x, rel), NA)
})

test_that("0 < sd", {
  x <- rnorm(4)
  sd_a <- sd(x) * -1
  sd_b <- sd(x)
  rel <- 0.9
  expect_error(calc_semci(x, sd_a, rel))
  expect_error(calc_semci(x, sd_b, rel), NA)
})

test_that("0 < rel < 1", {
  x <- rnorm(4)
  sd_x <- sd(x)
  rel_a <- letters[1]
  rel_b <- -1
  rel_c <- seq(0.1, 0.4, 0.1)
  expect_error(calc_semci(x, sd_x, rel_a))
  expect_error(calc_semci(x, sd_x, rel_b))
  expect_error(calc_semci(x, sd_x, rel_c), NA)
})

test_that("0 < se0", {
  x <- rnorm(4)
  sd_x <- sd(x)
  rel <- 0.9
  se0_a <- -1
  expect_error(calc_semci(x, sd_x, rel, se0_a))
  expect_error(calc_semci(x, sd_x, rel), NA)
})

test_that("0 < p_ci < 1", {
  x <- rnorm(4)
  sd_x <- sd(x)
  rel <- 0.9
  se0 <- NA
  p_ci_a <- -1
  p_ci_b <- 0.4
  expect_error(calc_semci(x, sd_x, rel, se0, p_ci_a))
  expect_error(calc_semci(x, sd_x, rel, se0, p_ci_b), NA)
})

test_that("dist must be one of 'normal', 't', or 'none'", {
  x <- rnorm(4)
  sd_x <- sd(x)
  rel <- 0.9
  dist_a <- "bla"
  dist_b <- "normal"
  expect_error(calc_semci(x, sd_x, rel, dist = dist_a))
  expect_error(calc_semci(x, sd_x, rel, dist = dist_b), NA)
})

test_that("if dist = 't', df > 1", {
  x <- rnorm(4)
  sd_x <- sd(x)
  rel <- 0.9
  expect_error(calc_semci(x, sd_x, rel, dist = "t"))
  expect_error(calc_semci(x, sd_x, rel, dist = "t", df = 0))
  expect_error(calc_semci(x, sd_x, rel, dist = "t", df = 2), NA)
})
