source("C:/Users/hyshim05/Desktop/hw-stat133/binomial/R/bin_choose.R")

context("test for checker functions")

test_that("check_prob fails with a prob value that is not between 0 and 1", {
  expect_error(check_prob(-0.5))
  expect_error(check_prob(1.2))
})

test_that("check_prob fails with a prob value that is not of length 1", {

  expect_error(check_prob(c(1:3)))
}
)

