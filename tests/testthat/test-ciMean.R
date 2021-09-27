test_that("ciMean output matches confint", {
  dat <- rnorm(10)
  expect_equal(
    object = unname(ciMean(dat)),
    expected = unname(confint(lm(dat ~ 1)))
  )
})
