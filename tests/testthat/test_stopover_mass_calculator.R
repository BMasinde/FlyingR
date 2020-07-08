context("stopover mass calculator")

test_that("missing file and data throws error", {
  expect_error(stopover.mass.calculator(data = NULL
  ))
})

data <- data("birds")
test_that("negaative duration throws error", {
  expect_error(stopover.mass.calculator(data = data, duration = -2
  ))
})

