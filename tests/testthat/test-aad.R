test_that("aad errors on non-numeric input", {
  expect_error(aad(x = "c"))
})
