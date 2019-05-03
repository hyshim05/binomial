library(testthat)

context("test for bin_probability function")

test_that("bin_probability ok", {
  x <- 5
  y <- 2
  z <- 0.5

  expect_error(bin_probability(x, y, z))
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_length(bin_probability(2, 5, 0.5), 1)

})

