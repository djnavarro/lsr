test_that("ciMean output matches confint", {
  dat <- rnorm(10)

  # same results at default .95 level
  expect_equal(
    object = unname(ciMean(dat)),
    expected = unname(confint(lm(dat ~ 1)))
  )

  # same results at modified level
  expect_equal(
    object = unname(ciMean(dat, conf = .5)),
    expected = unname(confint(lm(dat ~ 1), level = .5))
  )
})

test_that("ciMean respects na.rm argument", {

  dat1 <- rnorm(10)
  dat2 <- c(dat1, NA)

  expect_equal(
    object = unname(ciMean(dat2, na.rm = TRUE)),
    expected = unname(ciMean(dat1))
  )

  expect_equal(
    object = unname(ciMean(dat2, na.rm = FALSE)),
    expected = matrix(c(NA, NA), nrow = 1, ncol = 2)
  )

})

test_that("ciMean errors when passed character or factor", {

  expect_error(ciMean(c("a", "b", "c")))
  expect_error(ciMean(factor(c("a", "b", "c"))))

})

test_that("ciMean succeeds when passed data frame", {

  df <- data.frame(
    num = 1:4,
    txt = c("a", "b", "c", "d")
  )
  ci_df <- ciMean(df)

  expect_type(ci_df, "double")
  expect_equal(dim(ci_df), c(2, 2))
  expect_equal(ci_df[1,,drop = FALSE], with(df, ciMean(num)))

})

test_that("ciMean succeeds when passed numeric matrix", {

  mat <- matrix(1:18, 6, 3)
  expect_true(is.matrix(ciMean(mat)))
  expect_equal(dim(ciMean(mat)), c(3, 2))

})


