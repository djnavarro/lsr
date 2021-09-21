
# use n large enough to ensure we never have a cell count problem
df <- data.frame(
  gender = factor(sample(c("male", "female"), size = 100, replace = TRUE)),
  answer = factor(sample(c("head", "tail"), size = 100, replace = TRUE))
)

test_that("associationTest computes the same p-value as stats::chisq.test", {
  expect_equal(
    object = (associationTest(~ gender + answer, df))$p.value,
    expected = (chisq.test(df$gender, df$answer))$p.value
  )
})

test_that("associationTest returns the same results regardless of data context", {

  assoc <- associationTest(~ gender + answer, df)
  gender <- df$gender
  answer <- df$answer
  df <- "placeholder_in_local_scope"

  expect_equal(
    object = associationTest(~ gender + answer),
    expected = assoc
  )
})



test_that("associationTest warns for small N", {

  df <- data.frame(
    gender = as.factor(c("male", "female", "male", "female", "nonbinary")),
    answer = as.factor(c("head", "tail", "tail", "head", "head"))
  )

  expect_warning(associationTest(~ gender + answer, df), "frequencies too small")

})



