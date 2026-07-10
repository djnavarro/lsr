
# rmAll() operates on parent.frame() and its ask = FALSE path bypasses
# the interactive readline prompt. We call it from anonymous functions
# so that its parent frame contains exactly the variables we intend to test.

test_that("rmAll removes all objects from the calling environment", {
  removed <- (function() {
    x <- 1
    y <- 2
    z <- 3
    rmAll(ask = FALSE)
    c(exists("x"), exists("y"), exists("z"))
  })()
  expect_equal(removed, c(FALSE, FALSE, FALSE))
})

test_that("rmAll returns 1 invisibly after removing objects", {
  ret <- (function() {
    x <- 42
    rmAll(ask = FALSE)
  })()
  expect_equal(ret, 1L)
})

test_that("rmAll returns 1 invisibly when workspace is already empty", {
  # When ask = FALSE and no objects exist, rm() is called on an empty list
  # (a no-op); the function still returns 1 invisibly.
  ret <- (function() rmAll(ask = FALSE))()
  expect_equal(ret, 1L)
})
