gender <- factor(c(
  "male", "male", "male", "male", "female", "female",
  "female", "male", "male", "male"
))

test_that("goodnessOfFitTest returns a gofTest object", {
  # Regression guard for the original test
  expect_s3_class(goodnessOfFitTest(gender), "gofTest")
})

test_that("goodnessOfFitTest warns for small expected frequencies", {
  # Regression guard for the original test
  expect_warning(goodnessOfFitTest(gender, p = c(.4, .6)), "chi-squared approximation")
})

test_that("goodnessOfFitTest p-value matches chisq.test", {
  # Regression guard for the original test
  expect_equal(
    goodnessOfFitTest(gender)$p.value,
    chisq.test(table(gender))$p.value
  )
})

test_that("goodnessOfFitTest result has expected fields", {
  result <- goodnessOfFitTest(gender)
  expect_named(result, c(
    "outcome", "p", "observed", "expected", "difference",
    "statistic", "df", "p.value", "warn"
  ))
})

test_that("goodnessOfFitTest uses equal probabilities by default", {
  result <- goodnessOfFitTest(gender)
  # Two levels → each probability should be 0.5
  expect_equal(unname(result$p), c(0.5, 0.5))
})

test_that("goodnessOfFitTest accepts a named p vector matched to factor levels", {
  # Names in opposite order to factor levels — should be reordered automatically.
  # suppressWarnings: small N also triggers "expected frequencies too small".
  result <- suppressWarnings(goodnessOfFitTest(gender, p = c(male = 0.6, female = 0.4)))
  expect_equal(unname(result$p), c(0.4, 0.6)) # female first (level order)
})

test_that("goodnessOfFitTest warns when p does not sum to 1 but still runs", {
  expect_warning(
    goodnessOfFitTest(gender, p = c(0.3, 0.3)),
    "do not add up to 1"
  )
  # Should return a result despite the warning
  result <- suppressWarnings(goodnessOfFitTest(gender, p = c(0.3, 0.3)))
  expect_s3_class(result, "gofTest")
})

test_that("goodnessOfFitTest warns when missing data are removed", {
  # N large enough that expected frequencies won't trigger a second warning
  gender_na <- factor(c(rep("male", 12), rep("female", 8), NA, NA))
  expect_warning(goodnessOfFitTest(gender_na), "removed due to missingness")
})

test_that("goodnessOfFitTest errors when x is not a factor", {
  expect_error(goodnessOfFitTest(c("male", "female", "male")), "must be a factor")
  expect_error(goodnessOfFitTest(1:5), "must be a factor")
})

test_that("goodnessOfFitTest errors when p has the wrong length", {
  expect_error(
    goodnessOfFitTest(gender, p = c(0.25, 0.25, 0.5)),
    "wrong number of elements"
  )
})

test_that("goodnessOfFitTest errors when factor has only one level", {
  x <- factor(c("a", "a", "a"))
  expect_error(goodnessOfFitTest(x), "at least two levels")
})

test_that("print.gofTest returns the original object invisibly", {
  result <- goodnessOfFitTest(gender)
  out <- capture.output(ret <- print(result))
  expect_identical(ret, result)
})

test_that("print.gofTest output contains expected section headings", {
  result <- goodnessOfFitTest(gender)
  out <- capture.output(print(result))
  expect_true(any(grepl("Chi-square test against specified probabilities", out)))
  expect_true(any(grepl("Hypotheses", out)))
  expect_true(any(grepl("Descriptives", out)))
  expect_true(any(grepl("Test results", out)))
})
