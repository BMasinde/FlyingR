
context("Path to data arguments check")


path <- "/Users/masinde/Documents/R projects/Flight project/Preset_birds.csv"

test_that("Wrong argument in fileArgs", {
  expect_error(.pathToData(fileArgs = list(path, X1 = "random")))
})

test_that("Missing path throws error", {
  expect_error(.pathToData(fileArgs = list(header = TRUE)))
})
