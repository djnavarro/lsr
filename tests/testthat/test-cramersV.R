test_that("cramersV returns the existing 3x2 example value", {
  # Regression guard for the original single test
  condition1 <- c(30, 20, 50)
  condition2 <- c(35, 30, 35)
  X <- cbind(condition1, condition2)
  rownames(X) <- c("choice1", "choice2", "choice3")
  expect_equal(cramersV(X), 0.1586139, tolerance = 1e-6)
})

test_that("cramersV returns a single numeric value", {
  X <- cbind(c(30, 20, 50), c(35, 30, 35))
  result <- cramersV(X)
  expect_type(result, "double")
  expect_length(result, 1L)
})

test_that("cramersV gives the correct value for a 2x2 table", {
  # chisq.test applies Yates' continuity correction for 2x2 tables by default,
  # so the result is sqrt(7.22/200) ≈ 0.19, not the naive sqrt(8/200) = 0.2
  X <- cbind(c(40, 60), c(60, 40))
  expect_equal(cramersV(X), sqrt(7.22 / 200), tolerance = 1e-6)
})

test_that("cramersV accepts two factor/character vectors as input", {
  # cramersV passes ... to chisq.test, which accepts two vectors.
  # Use N large enough to avoid the "approximation may be incorrect" warning.
  set.seed(9173)
  x <- factor(sample(c("a", "b"), 80, replace = TRUE))
  y <- factor(sample(c("x", "y"), 80, replace = TRUE))
  result <- cramersV(x, y)
  expect_type(result, "double")
  expect_length(result, 1L)
  # Should match computing via a table
  expect_equal(cramersV(x, y), cramersV(table(x, y)), tolerance = 1e-8)
})

test_that("cramersV equals 0 for a perfectly uniform distribution", {
  # If both columns are identical, chi-square = 0 → V = 0
  X <- cbind(c(50, 50), c(50, 50))
  expect_equal(cramersV(X), 0, tolerance = 1e-8)
})

test_that("cramersV result is between 0 and 1", {
  X <- cbind(c(30, 20, 50), c(35, 30, 35))
  v <- cramersV(X)
  expect_gte(v, 0)
  expect_lte(v, 1)
})

test_that("cramersV goodness-of-fit branch: single vector with probability argument", {
  # chisq.test(x, p=...) triggers the GOF branch in cramersV
  observed <- c(10, 20, 30)
  probs <- c(1 / 6, 2 / 6, 3 / 6)
  v <- suppressWarnings(cramersV(observed, p = probs))
  expect_type(v, "double")
  expect_length(v, 1L)
  expect_gte(v, 0)
  expect_lte(v, 1)
})
