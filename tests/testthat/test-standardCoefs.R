X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
Y <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)
X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33)

model1 <- lm(Y ~ X1)
model2 <- lm(Y ~ X1 + X2)
model3 <- lm(Y ~ X1 * X2)

test_that("standardCoefs example works", {
  std1 <- standardCoefs(model1)
  expect_equal(std1[2], 0.8674478, tolerance = 1e-6)

  std2 <- standardCoefs(model2)
  expect_equal(std2[1, 2], 0.84584304, tolerance = 1e-4)
  expect_equal(std2[2, 2], -0.08903252, tolerance = 1e-4)

  std3 <- standardCoefs(model3)
  expect_equal(std3[1, 2], 0.5958602, tolerance = 1e-4)
  expect_equal(std3[2, 2], -0.3701504, tolerance = 1e-4)
  expect_equal(std3[3, 2], 0.3562668, tolerance = 1e-4)
})

test_that("standardCoefs returns a matrix with columns b and beta", {
  result <- standardCoefs(model1)
  expect_true(is.matrix(result))
  expect_equal(colnames(result), c("b", "beta"))
})

test_that("standardCoefs b column matches lm coefficients (excluding intercept)", {
  result <- standardCoefs(model2)
  lm_coefs <- coef(model2)[-1] # drop intercept
  expect_equal(result[, "b"], lm_coefs, tolerance = 1e-10)
})

test_that("standardCoefs row names match predictor names", {
  result <- standardCoefs(model2)
  expect_equal(rownames(result), c("X1", "X2"))
})

test_that("standardCoefs errors when x is not an lm object", {
  expect_error(standardCoefs(list(coefficients = 1:3)), '"x" must be a linear model object')
  expect_error(standardCoefs("not a model"), '"x" must be a linear model object')
  # Note: aov objects inherit from lm in R, so aov input does NOT error here
})
