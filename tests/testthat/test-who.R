
# who() inspects parent.frame(), so we test it by calling it from within
# helper functions whose local variables are exactly what we want to observe.

test_that("who returns a whoList object", {
  result <- (function() {
    x <- 1:10
    who()
  })()
  expect_s3_class(result, "whoList")
})

test_that("who returns a data frame with Name, Class, Size columns", {
  result <- (function() {
    x <- 1:10
    who()
  })()
  expect_named(result, c("Name", "Class", "Size"))
})

test_that("who lists variables present in the calling environment", {
  result <- (function() {
    cats <- 4L
    mood <- "happy"
    who()
  })()
  expect_true("cats" %in% result$Name)
  expect_true("mood" %in% result$Name)
})

test_that("who returns a zero-length whoList for an empty environment", {
  result <- (function() who())()
  expect_s3_class(result, "whoList")
  expect_equal(length(result), 0L)
})

test_that("who expand = TRUE includes data frame columns prefixed with $", {
  result <- (function() {
    df <- data.frame(a = 1:3, b = letters[1:3])
    who(expand = TRUE)
  })()
  expect_true(any(grepl("\\$a", result$Name)))
  expect_true(any(grepl("\\$b", result$Name)))
})

test_that("who expand = FALSE does not expand data frame columns", {
  result <- (function() {
    df <- data.frame(a = 1:3, b = letters[1:3])
    who(expand = FALSE)
  })()
  expect_false(any(grepl("\\$", result$Name)))
  expect_true("df" %in% result$Name)
})

test_that("who reports correct class for common types", {
  result <- (function() {
    num <- 1.5
    chr <- "hello"
    lgl <- TRUE
    who()
  })()
  expect_equal(result$Class[result$Name == "chr"], "character")
  expect_equal(result$Class[result$Name == "lgl"], "logical")
  expect_equal(result$Class[result$Name == "num"], "numeric")
})

test_that("print.whoList returns its input invisibly", {
  w <- (function() { x <- 1:5; who() })()
  out <- capture.output(ret <- print(w))
  expect_identical(ret, w)
})

test_that("print.whoList prints 'No variables found' for an empty whoList", {
  w <- (function() who())()
  out <- capture.output(print(w))
  expect_true(any(grepl("No variables found", out)))
})

test_that("who errors on invalid expand argument", {
  expect_error(who(expand = NA),            '"expand" must be a single logical value')
  expect_error(who(expand = "yes"),         '"expand" must be a single logical value')
  expect_error(who(expand = c(TRUE, FALSE)),'"expand" must be a single logical value')
})
