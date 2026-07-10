set.seed(1)
x <- data.frame(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100)
)
y <- data.frame(
  d = rnorm(100),
  e = rnorm(100),
  f = rnorm(100)
)
z <- data.frame(
  g = sample(letters, 100, TRUE),
  h = rnorm(100),
  i = rnorm(100)
)
z$h[1:5] <- NA

test_that("correlation returns s3 object", {
  expect_s3_class(correlate(x),    "correlate")
  expect_s3_class(correlate(x, y), "correlate")
  expect_s3_class(correlate(x, z), "correlate")
})

out <- list(
  xx = unclass(correlate(x)),
  xy = unclass(correlate(x, y)),
  xz = unclass(correlate(x, z))
)

test_that("correlation output has expected structure", {
  for (m in seq_along(out)) {
    expect_type(out[[m]], "list")
    expect_length(out[[m]], 5)
    expect_named(out[[m]], c("correlation","p.value","sample.size","args","tiesProblem"))
    for (i in 1:3) expect_true(inherits(out[[m]][[i]], "matrix"))
    expect_type(out[[m]][[4]], "character")
    expect_type(out[[m]][[5]], "logical")
  }
})

test_that("NA values appear in correct slots", {
  # All correlations with character-valued z$g are NA
  expect_true(all(is.na(out$xz$correlation[, "g"])))
  # Sample sizes adjusted correctly
  expect_equal(out$xz$sample.size[1, "g"], NA_integer_)
  expect_equal(out$xz$sample.size[1, "h"], 95L)
  expect_equal(out$xz$sample.size[1, "i"], 100L)
})

test_that("correlate Pearson values match cor() for complete numeric data", {
  result <- correlate(x)
  expect_equal(result$correlation["a", "b"], cor(x$a, x$b), tolerance = 1e-10)
  expect_equal(result$correlation["a", "c"], cor(x$a, x$c), tolerance = 1e-10)
  expect_equal(result$correlation["b", "c"], cor(x$b, x$c), tolerance = 1e-10)
})

test_that("correlate two-matrix Pearson values match cor()", {
  result <- correlate(x, y)
  for (xi in names(x)) {
    for (yi in names(y)) {
      expect_equal(
        result$correlation[xi, yi],
        cor(x[[xi]], y[[yi]]),
        tolerance = 1e-10
      )
    }
  }
})

test_that("correlate diagonal is NA (no self-correlation computed) in one-matrix case", {
  result <- correlate(x)
  # The one-matrix loop only fills off-diagonal pairs; diagonal stays NA
  expect_true(all(is.na(diag(result$correlation))))
})

test_that("correlate with corr.method = spearman gives Spearman coefficients", {
  result <- correlate(x, corr.method = "spearman")
  expect_equal(
    result$correlation["a", "b"],
    cor(x$a, x$b, method = "spearman"),
    tolerance = 1e-10
  )
})

test_that("print.correlate returns its input invisibly", {
  r   <- correlate(x)
  out <- capture.output(ret <- print(r))
  expect_identical(ret, r)
})

test_that("print.correlate output includes CORRELATIONS header", {
  r   <- correlate(x)
  out <- capture.output(print(r))
  expect_true(any(grepl("CORRELATIONS", out)))
})

test_that("print.correlate with test = TRUE shows p-values and sample sizes", {
  r   <- correlate(x, test = TRUE)
  out <- capture.output(print(r))
  expect_true(any(grepl("p-VALUES",    out)))
  expect_true(any(grepl("SAMPLE SIZES", out)))
})

test_that("correlate accepts a numeric vector as x (two-matrix form)", {
  set.seed(4812)
  a <- rnorm(30)
  b <- data.frame(c = rnorm(30), d = rnorm(30))
  r <- correlate(a, b)
  expect_s3_class(r, "correlate")
  expect_equal(r$correlation["a", "c"], cor(a, b$c), tolerance = 1e-10)
})

test_that("correlate accepts a numeric vector as y", {
  set.seed(3561)
  a <- data.frame(c = rnorm(30), d = rnorm(30))
  b <- rnorm(30)
  r <- correlate(a, b)
  expect_s3_class(r, "correlate")
  expect_equal(r$correlation["c", "b"], cor(a$c, b), tolerance = 1e-10)
})

test_that("correlate accepts a numeric matrix as x", {
  set.seed(7203)
  m <- matrix(rnorm(60), 20, 3, dimnames = list(NULL, c("a", "b", "c")))
  r <- correlate(m)
  expect_s3_class(r, "correlate")
  expect_equal(r$correlation["a", "b"], cor(m[, "a"], m[, "b"]), tolerance = 1e-10)
})

test_that("correlate with a single numeric vector returns a well-formed object", {
  set.seed(9014)
  v <- rnorm(20)
  r <- correlate(v)
  expect_s3_class(r, "correlate")
  expect_equal(dim(r$correlation), c(1L, 1L))
  # diagonal sample size is filled; correlation is NA (no off-diagonal pairs)
  expect_equal(r$sample.size[1, 1], 20L)
  expect_true(is.na(r$correlation[1, 1]))
})

test_that("correlate errors when x is not a matrix, data frame, or numeric vector", {
  expect_error(correlate(list(a = 1:5, b = 1:5)), "x must be a matrix or data frame")
})

test_that("correlate errors when y is not a matrix, data frame, or numeric vector", {
  df <- data.frame(a = 1:5, b = 1:5)
  expect_error(correlate(df, list(c = 1:5)), "y must be a matrix or data frame")
})
