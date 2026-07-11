test_that("quantileCut returns a factor", {
  x <- c(0, 1, 2, 3, 4, 5, 7, 10, 15)
  expect_s3_class(quantileCut(x, 3), "factor")
})

test_that("quantileCut returns the requested number of levels", {
  x <- 1:20
  expect_equal(nlevels(quantileCut(x, 4)), 4L)
  expect_equal(nlevels(quantileCut(x, 5)), 5L)
})

test_that("quantileCut produces approximately equal-sized bins", {
  # With clean data, each bin should contain exactly n/k observations
  x <- 1:100
  bins <- quantileCut(x, 4)
  counts <- as.integer(table(bins))
  expect_true(all(counts >= 24L & counts <= 26L))
})

test_that("quantileCut matches the existing iris example", {
  # Keeps the original passing assertion as a regression guard
  expect_equal(
    tabulate(quantileCut(iris$Sepal.Length, 3)),
    c(52, 56, 42)
  )
})

test_that("quantileCut passes NA values through to the output", {
  x <- c(1, 2, NA, 4, 5)
  result <- quantileCut(x, 2)
  expect_true(is.na(result[3]))
})

test_that("quantileCut passes extra arguments to cut (e.g. labels)", {
  x <- 1:9
  result <- quantileCut(x, 3, labels = c("low", "mid", "high"))
  expect_equal(levels(result), c("low", "mid", "high"))
})

test_that("quantileCut errors when x is not numeric", {
  expect_error(quantileCut(c("a", "b", "c"), 3), '"x" must be a numeric vector')
  expect_error(quantileCut(factor(1:5), 2), '"x" must be a numeric vector')
})

test_that("quantileCut errors when n is not a single number", {
  expect_error(quantileCut(1:10, c(2, 3)), 'number of bins "n" must be a single number')
  expect_error(quantileCut(1:10, "three"), 'number of bins "n" must be a single number')
})
