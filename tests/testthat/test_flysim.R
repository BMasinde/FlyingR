
context("data input in flysim")

data("birds")

vec_data <- c(5, 4, 7, 8, 9, 10)

test_that("vector input is not allowed", {
  expect_error(flysim(vec_data))
})

test_that("wrong method? throw error", {
  expect_error(flysim(data = birds, method = "bregt"))
})

