
# use n large enough to ensure we never have a cell count problem
df <- data.frame(
  gender = factor(sample(c("male", "female"), size = 100, replace = TRUE)),
  answer = factor(sample(c("head", "tail"), size = 100, replace = TRUE))
)

test_that("associationTest returns an assocTest object", {
  expect_s3_class(associationTest(~ gender + answer, df), "assocTest")
})

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
  df_smol <- data.frame(
    gender = as.factor(c("male", "female", "male", "female", "nonbinary")),
    answer = as.factor(c("head", "tail", "tail", "head", "head"))
  )
  expect_warning(associationTest(~ gender + answer, df_smol), "frequencies too small")
})

test_that("associationTest print method returns original object", {
  x <- associationTest(~gender + answer, df)
  sink(tempfile()) # don't clutter the output
  y <- print(x)
  sink()
  expect_identical(x, y)
})


test_that("associationTest print method contains expected lines", {
  tst <- associationTest(~gender + answer, df)
  out <- capture.output(print(tst))
  exists_pattern <- function(strs, pattern) {
    length(grep(pattern = pattern, x = strs)) > 0
  }

  expect_true(exists_pattern(out, "Chi-square test of categorical association"))
  expect_true(exists_pattern(out, "Hypotheses"))
  expect_true(exists_pattern(out, "Observed contingency table"))
  expect_true(exists_pattern(out, "Expected contingency table"))
  expect_true(exists_pattern(out, "Test results"))
})






