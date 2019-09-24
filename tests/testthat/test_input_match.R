
context("Data columns check")

data("birds")

mult_col <- birds

mis_col <- birds

colnames(mult_col) <- c("Scientific.name", "Empty.mass", "Wing.span",
                        "Fat.mass", "name", "Order" )

colnames(mis_col) <- c("Scientific.name", "Empty.mass", "Wing.span",
                        "Fat.mass", "X1", "Wing.area")

test_that("Multiple column match throw error", {
  expect_error(flysim(data = mult_col))
})


test_that("Missing column match throws error", {
  expect_error(flysim(data = mis_col))
})

bad_factor <- birds[, -5]

bad_factor$ordo <-  as.factor(rep(c(3,2), 14))

test_that("Factor in ordo other 1 and 2 throws error", {
  expect_error(flysim(data = bad_factor))
})
