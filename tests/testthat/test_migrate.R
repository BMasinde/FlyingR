
context("Migrate")

data <- data("birds")

test_that("out of bounds protein_met throws error", {
  expect_error(migtate(data = data, method = "cmm",
                       speed_control = 1, min_energy_protein = 1,2
                                    ))
})

test_that("speed control is binary", {
  expect_error(migtate(data = data, method = "cmm",
                       speed_control = "constant_speed", min_energy_protein = 0.05
  ))
})

birds[5, 4] <- 0
test_that("zero fat mass throw error in migrate", {
  expect_error(migtate(
    data = data,
    method = "cmm",
    speed_control = 0,
    min_energy_protein = 0.05
  )
)
})
