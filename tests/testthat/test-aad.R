test_that("aad errors on non-numeric input", {
  expect_error(aad(x = "c"))
  expect_error(aad(x = c(list(1))))
})

test_that("aad respects na.rm", {
  expect_equal(aad(c(1, 3, NA), na.rm = FALSE), NA_real_)
  expect_equal(aad(c(1, 3, NA), na.rm = TRUE), 1)

  expect_error(aad(c(1, 3, NA), na.rm = NA))
  expect_error(aad(c(1, 3, NA), na.rm = NULL))
  expect_error(aad(c(1, 3, NA), na.rm = list(TRUE)))
  expect_error(aad(c(1, 3, NA), na.rm = c(TRUE, FALSE)))
})

test_that("aad returns correct numeric values", {
  # Docstring example: mean(|{1,3,6} - 10/3|) = 16/9
  expect_equal(aad(c(1, 3, 6)), 16/9, tolerance = 1e-6)

  # Uniform values: aad should be 0
  expect_equal(aad(c(4, 4, 4, 4)), 0)

  # Single element: aad should be 0
  expect_equal(aad(5), 0)

  # Symmetric sequence 1:5: mean(|{1,2,3,4,5} - 3|) = (2+1+0+1+2)/5 = 6/5
  expect_equal(aad(1:5), 6/5, tolerance = 1e-10)
})

test_that("aad returns a single numeric value", {
  expect_type(aad(1:10), "double")
  expect_length(aad(1:10), 1L)
})
