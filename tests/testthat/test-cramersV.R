test_that("cramersV example works", {

  condition1 <- c(30, 20, 50)
  condition2 <- c(35, 30, 35)
  X <- cbind( condition1, condition2 )
  rownames(X) <- c( 'choice1', 'choice2', 'choice3' )

  expect_equal(cramersV(X), 0.1586139, tolerance = .0000001)
})
