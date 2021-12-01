test_that("quantileCut sorta works", {
  expect_equal(
    tabulate(quantileCut(iris$Sepal.Length, 3)),
    c(52, 56, 42)
  )
})
