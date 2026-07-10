test_that("colCopy produces a matrix with x in each column", {
  x <- c(3, 1, 4, 1, 5)
  m <- colCopy(x, 4)
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(5L, 4L))
  # each column is identical to x
  for (j in 1:4) expect_equal(m[, j], x)
})

test_that("rowCopy produces a matrix with x in each row", {
  x <- c(3, 1, 4, 1, 5)
  m <- rowCopy(x, 4)
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(4L, 5L))
  # each row is identical to x
  for (i in 1:4) expect_equal(m[i, ], x)
})

test_that("colCopy and rowCopy are transposes of each other", {
  x <- 1:6
  expect_equal(colCopy(x, 3), t(rowCopy(x, 3)))
})

test_that("colCopy and rowCopy respect dimnames argument", {
  x <- c(3, 1, 4, 1, 5)
  dnames <- list(rows = c("r1", "r2", "r3"), cols = c("c1", "c2", "c3", "c4", "c5"))
  m <- rowCopy(x, 3, dnames)
  expect_equal(rownames(m), c("r1", "r2", "r3"))
  expect_equal(colnames(m), c("c1", "c2", "c3", "c4", "c5"))
})

test_that("colCopy and rowCopy inherit names from x when no dimnames given", {
  x <- c(a = 1, b = 2, c = 3)
  # colCopy: x names become row names
  mc <- colCopy(x, 2)
  expect_equal(rownames(mc), c("a", "b", "c"))
  # rowCopy: x names become column names
  mr <- rowCopy(x, 2)
  expect_equal(colnames(mr), c("a", "b", "c"))
})

test_that("colCopy errors on non-vector x", {
  # matrices fail is.vector() because they have a dim attribute
  expect_error(colCopy(matrix(1:4, 2, 2), 3), '"x" must be a vector')
  expect_error(colCopy(data.frame(a = 1), 3), '"x" must be a vector')
})

test_that("rowCopy errors on non-vector x", {
  expect_error(rowCopy(matrix(1:4, 2, 2), 3), '"x" must be a vector')
  expect_error(rowCopy(data.frame(a = 1), 3), '"x" must be a vector')
})

test_that("colCopy and rowCopy error when times is not a single number", {
  x <- 1:4
  expect_error(colCopy(x, "three"))
  expect_error(colCopy(x, c(2, 3)))
  expect_error(rowCopy(x, "three"))
  expect_error(rowCopy(x, c(2, 3)))
})
