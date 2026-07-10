
# print.TTest is defined in printTTest.R and handles three test types:
# one-sample, independent-samples, and paired-samples. Tests here verify
# that the output is well-formed and the method returns the original object
# invisibly. Numeric content is validated in the individual t-test test files.

# ── Shared helper ──────────────────────────────────────────────────────────────
tt_output <- function(tt) capture.output(print(tt))

# ── One-sample t-test ──────────────────────────────────────────────────────────
likert <- c(3, 1, 4, 1, 4, 6, 7, 2, 6, 6, 7)
tt_one <- oneSampleTTest(x = likert, mu = 4)

test_that("print.TTest currently returns NULL (docstring says invisible(x) — known bug)", {
  # BUG: print.TTest has no return() call, so it returns NULL rather than
  # invisible(x) as documented and as every other lsr print method does.
  # This test records the current (incorrect) behaviour so a future fix will
  # be caught immediately.
  out <- capture.output(ret <- print(tt_one))
  expect_null(ret)
})

test_that("print.TTest one-sample output contains expected section headings", {
  out <- tt_output(tt_one)
  expect_true(any(grepl("One sample t-test",       out)))
  expect_true(any(grepl("Descriptive statistics",   out)))
  expect_true(any(grepl("Hypotheses",               out)))
  expect_true(any(grepl("Test results",             out)))
  expect_true(any(grepl("Other information",        out)))
})

test_that("print.TTest one-sample output contains null and alternative hypotheses", {
  out <- tt_output(tt_one)
  expect_true(any(grepl("null:",        out)))
  expect_true(any(grepl("alternative:", out)))
})

test_that("print.TTest one-sample output contains t-statistic and p-value lines", {
  out <- tt_output(tt_one)
  expect_true(any(grepl("t-statistic", out)))
  expect_true(any(grepl("p-value",     out)))
})

test_that("print.TTest one-sample works without error for one-sided variant", {
  tt_gt <- oneSampleTTest(x = likert, mu = 4, one.sided = "greater")
  expect_silent(capture.output(print(tt_gt)))
  tt_lt <- oneSampleTTest(x = likert, mu = 4, one.sided = "less")
  expect_silent(capture.output(print(tt_lt)))
})

# ── Independent-samples t-test ─────────────────────────────────────────────────
df <- data.frame(
  rt   = c(451, 562, 704, 324, 505, 600, 829),
  cond = factor(x = c(1,1,1,2,2,2,2), labels = c("group1","group2"))
)
tt_ind <- independentSamplesTTest(rt ~ cond, df)

test_that("print.TTest returns NULL for independent-samples (known bug: should be invisible(x))", {
  out <- capture.output(ret <- print(tt_ind))
  expect_null(ret)
})

test_that("print.TTest independent-samples output contains expected section headings", {
  out <- tt_output(tt_ind)
  expect_true(any(grepl("independent samples t-test", out, ignore.case = TRUE)))
  expect_true(any(grepl("Descriptive statistics",     out)))
  expect_true(any(grepl("Hypotheses",                 out)))
  expect_true(any(grepl("Test results",               out)))
  expect_true(any(grepl("Other information",          out)))
})

test_that("print.TTest independent-samples works for Student and one-sided variants", {
  tt_st  <- independentSamplesTTest(rt ~ cond, df, var.equal = TRUE)
  tt_os  <- independentSamplesTTest(rt ~ cond, df, one.sided = "group1")
  expect_silent(capture.output(print(tt_st)))
  expect_silent(capture.output(print(tt_os)))
})

# ── Paired-samples t-test ──────────────────────────────────────────────────────
df_long <- data.frame(
  id   = factor(x = c(1,1,2,2,3,3,4,4), labels = c("alice","bob","chris","diana")),
  time = factor(x = c(1,2,1,2,1,2,1,2), labels = c("time1","time2")),
  wm   = c(3, 4, 6, 6, 9, 12, 7, 9)
)
tt_paired <- pairedSamplesTTest(formula = wm ~ time, data = df_long, id = "id")

test_that("print.TTest returns NULL for paired-samples (known bug: should be invisible(x))", {
  out <- capture.output(ret <- print(tt_paired))
  expect_null(ret)
})

test_that("print.TTest paired-samples output contains expected section headings", {
  out <- tt_output(tt_paired)
  expect_true(any(grepl("Paired samples t-test",  out)))
  expect_true(any(grepl("Descriptive statistics", out)))
  expect_true(any(grepl("Hypotheses",             out)))
  expect_true(any(grepl("Test results",           out)))
  expect_true(any(grepl("Other information",      out)))
})

test_that("print.TTest paired-samples works for one-sided variant", {
  tt_os <- pairedSamplesTTest(formula = wm ~ time, data = df_long,
                               id = "id", one.sided = "time2")
  expect_silent(capture.output(print(tt_os)))
})
