source("C:/Users/hyshim05/Desktop/hw-stat133/binomial/R/bin_probability.R")

context("test for bin_probability function")

test_that("bin_probability fails when k is greater than n", {
  x <- 5
  y <- 2
  z <- 0.5

  expect_error(bin_probability(x, y, z))

})

test_that("bin_probability fails with an invalid prob value",{
  a <- 2
  b <- 5

  expect_error(bin_probability(a, b, -0.5), "Invalid probability value, p has to be a number between 0 and 1")
  expect_error(bin_probability(a, b, 1.2), "Invalid probability value, p has to be a number between 0 and 1")
})
