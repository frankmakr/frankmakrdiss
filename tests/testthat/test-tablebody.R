test_that("input must be numeric", {
  x1 <- matrix(letters[1:9], ncol = 3)
  x2 <- matrix(rnorm(9), ncol = 3)
  expect_error(make_table_body(x1))
  expect_error(make_table_body(data.frame(x1)))
  expect_error(make_table_body(data.frame(x2)), NA)
  expect_error(make_table_body(data.frame(x1, x2)), NA)
})
