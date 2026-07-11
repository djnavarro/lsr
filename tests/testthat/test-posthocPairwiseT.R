dataset <- data.frame(
  outcome = c(1, 2, 3, 2, 3, 4, 5, 6, 7),
  group   = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c"))
)
anova1 <- aov(outcome ~ group, data = dataset)

test_that("posthocPairwiseT matches pairwise.t.test with default correction", {
  # Regression guard for the original test
  expect_equal(
    posthocPairwiseT(anova1),
    with(dataset, pairwise.t.test(outcome, group))
  )
})

test_that("posthocPairwiseT returns a pairwise.htest object", {
  expect_s3_class(posthocPairwiseT(anova1), "pairwise.htest")
})

test_that("posthocPairwiseT p-value matrix has the right dimensions", {
  result <- posthocPairwiseT(anova1)
  # Three groups → 2x2 lower-triangle p-value matrix (one NA in upper triangle)
  expect_true(is.matrix(result$p.value))
  expect_equal(dim(result$p.value), c(2L, 2L))
})

test_that("posthocPairwiseT passes p.adjust.method through to pairwise.t.test", {
  result_holm <- posthocPairwiseT(anova1, p.adjust.method = "holm")
  result_bonferroni <- posthocPairwiseT(anova1, p.adjust.method = "bonferroni")
  ref_holm <- with(dataset, pairwise.t.test(outcome, group, p.adjust.method = "holm"))
  ref_bonferroni <- with(dataset, pairwise.t.test(outcome, group, p.adjust.method = "bonferroni"))

  expect_equal(result_holm$p.value, ref_holm$p.value, tolerance = 1e-10)
  expect_equal(result_bonferroni$p.value, ref_bonferroni$p.value, tolerance = 1e-10)
})

test_that("posthocPairwiseT Bonferroni p-values are >= Holm p-values", {
  pv_holm <- posthocPairwiseT(anova1, p.adjust.method = "holm")$p.value
  pv_bonferroni <- posthocPairwiseT(anova1, p.adjust.method = "bonferroni")$p.value
  # Bonferroni is more conservative (or equal) to Holm
  expect_true(all(pv_bonferroni >= pv_holm, na.rm = TRUE))
})

test_that("posthocPairwiseT errors when x is not an aov object", {
  expect_error(posthocPairwiseT(lm(outcome ~ group, data = dataset)), '"x" must be an aov object')
  expect_error(posthocPairwiseT(list(a = 1)), '"x" must be an aov object')
  expect_error(posthocPairwiseT("not an aov"), '"x" must be an aov object')
})

test_that("posthocPairwiseT errors for two-way ANOVA", {
  dataset2 <- data.frame(
    outcome = c(1, 2, 3, 2, 3, 4, 5, 6, 7),
    group   = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c")),
    block   = factor(c("x", "y", "z", "x", "y", "z", "x", "y", "z"))
  )
  anova2 <- aov(outcome ~ group + block, data = dataset2)
  expect_error(posthocPairwiseT(anova2), "only supports one-way ANOVA")
})
