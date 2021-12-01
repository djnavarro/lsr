eyes1 <- c("green","green","brown","brown","blue")
eyes2 <- c("green","green","brown","brown","blue",NA,NA,NA)

test_that("mode examples work", {

  expect_equal(modeOf(eyes1), c("green", "brown"))
  expect_equal(maxFreq(eyes1), 2)

  expect_equal(modeOf(eyes2, na.rm = FALSE), NA)
  expect_equal(maxFreq(eyes2, na.rm = FALSE), 3)

  expect_equal(modeOf(eyes2, na.rm = TRUE), c("green", "brown"))
  expect_equal(maxFreq(eyes2, na.rm = TRUE), 2)

})
