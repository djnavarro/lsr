eyes1 <- c("green", "green", "brown", "brown", "blue")
eyes2 <- c("green", "green", "brown", "brown", "blue", NA, NA, NA)

test_that("modeOf returns the correct modal values", {
  expect_equal(modeOf(eyes1), c("green", "brown"))

  # With NA removed (default na.rm = TRUE): same as without NA
  expect_equal(modeOf(eyes2, na.rm = TRUE), c("green", "brown"))

  # With na.rm = FALSE and NA is most frequent: returns NA
  expect_equal(modeOf(eyes2, na.rm = FALSE), NA)

  # Unique mode (no tie)
  expect_equal(modeOf(c(1, 2, 2, 3)), 2)

  # Factor input
  f <- factor(c("a", "a", "b", "c"))
  expect_equal(modeOf(f), "a")
})

test_that("maxFreq returns the correct modal frequency", {
  expect_equal(maxFreq(eyes1), 2)
  expect_equal(maxFreq(eyes2, na.rm = FALSE), 3)
  expect_equal(maxFreq(eyes2, na.rm = TRUE), 2)
  expect_equal(maxFreq(c(1, 2, 2, 2, 3)), 3)
})

test_that("modeOf errors on invalid input types", {
  # is.vector(list(...)) is TRUE in R, so lists pass the guard; only
  # objects that fail is.vector() AND is.factor() trigger the error
  expect_error(modeOf(matrix(1:4, 2, 2)), '"x" must be a vector or a factor')
  expect_error(modeOf(data.frame(a = 1:3)), '"x" must be a vector or a factor')
})

test_that("modeOf errors on invalid na.rm argument", {
  expect_error(modeOf(eyes1, na.rm = NA))
  expect_error(modeOf(eyes1, na.rm = "yes"))
  expect_error(modeOf(eyes1, na.rm = c(TRUE, FALSE)))
})

test_that("maxFreq errors on invalid input types", {
  expect_error(maxFreq(matrix(1:4, 2, 2)), '"x" must be a vector or a factor')
  expect_error(maxFreq(data.frame(a = 1:3)), '"x" must be a vector or a factor')
})

test_that("maxFreq errors on invalid na.rm argument", {
  expect_error(maxFreq(eyes1, na.rm = NA))
  expect_error(maxFreq(eyes1, na.rm = "yes"))
  expect_error(maxFreq(eyes1, na.rm = c(TRUE, FALSE)))
})
