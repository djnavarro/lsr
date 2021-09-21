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

