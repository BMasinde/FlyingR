context("stopover mass calculator")

test_that("missing file and data throws error", {
  expect_error(stopover.mass.calculator(data = NULL
  ))
})
