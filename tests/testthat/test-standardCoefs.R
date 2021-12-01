

test_that("standardCoefs example works", {

  # Example 1: simple linear regression

  # data
  X1 <- c(0.69, 0.77, 0.92, 1.72, 1.79, 2.37, 2.64, 2.69, 2.84, 3.41)
  Y  <- c(3.28, 4.23, 3.34, 3.73, 5.33, 6.02, 5.16, 6.49, 6.49, 6.05)

  model1 <- lm( Y ~ X1 )  # run a simple linear regression
  std1 <- standardCoefs( model1 ) # extract standardised coefficients
  expect_equal(std1[2], 0.8674478, tolerance = .0000001)

  # Example 2: multiple linear regression

  X2 <- c(0.19, 0.22, 0.95, 0.43, 0.51, 0.04, 0.12, 0.44, 0.38, 0.33)
  model2 <- lm( Y ~ X1 + X2 )   # new model
  std2 <- standardCoefs( model2 )       # standardised coefficients
  expect_equal(std2[1,2], 0.84584304, tolerance = .0001)
  expect_equal(std2[2,2], -0.08903252, tolerance = .0001)

  #Example 3: interaction terms

  model3 <- lm( Y ~ X1 * X2 )
  std3 <- standardCoefs( model3 )
  expect_equal(std3[1,2], 0.5958602, tolerance = .0001)
  expect_equal(std3[2,2], -0.3701504, tolerance = .0001)
  expect_equal(std3[3,2], 0.3562668, tolerance = .0001)

})
