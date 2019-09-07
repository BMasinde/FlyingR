context("data input in method breguet")

test_that("Empty fat mass throws an error", {
  expect_error(.breguet(3.77, 1.6, 0, 2, 0.3))
})
