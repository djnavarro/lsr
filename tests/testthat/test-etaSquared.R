outcome <- c(1.4, 2.1, 3.0, 2.1, 3.2, 4.7, 3.5, 4.5, 5.4)
treatment1 <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))
treatment2 <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
anova1 <- aov(outcome ~ treatment1)
anova2 <- aov(outcome ~ treatment1 + treatment2)

test_that("etaSquared example works (regression guard)", {
  expect_equal(etaSquared(anova1)[1], 0.5497229, tolerance = 1e-6)
  expect_equal(etaSquared(anova1)[2], 0.5497229, tolerance = 1e-6)

  expect_equal(etaSquared(anova2)[1, 1], 0.5497229, tolerance = 1e-6)
  expect_equal(etaSquared(anova2)[2, 1], 0.4305727, tolerance = 1e-6)
  expect_equal(etaSquared(anova2)[1, 2], 0.9653961, tolerance = 1e-6)
  expect_equal(etaSquared(anova2)[2, 2], 0.9562393, tolerance = 1e-6)
})

test_that("etaSquared returns a matrix with the right column names", {
  result <- etaSquared(anova1)
  expect_true(is.matrix(result))
  expect_equal(colnames(result), c("eta.sq", "eta.sq.part"))
})

test_that("etaSquared values are between 0 and 1", {
  result <- etaSquared(anova2)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("etaSquared type = 1 and type = 2 agree for balanced one-way ANOVA", {
  # For a balanced one-way design, Type I and Type II SS are identical
  result1 <- etaSquared(anova1, type = 1)
  result2 <- etaSquared(anova1, type = 2)
  expect_equal(result1, result2, tolerance = 1e-10)
})

test_that("etaSquared type = 3 runs without error and returns a numeric matrix", {
  result <- etaSquared(anova1, type = 3)
  expect_true(is.matrix(result))
  expect_equal(colnames(result), c("eta.sq", "eta.sq.part"))
})

test_that("etaSquared anova = TRUE returns the extended table with 7 columns", {
  result <- etaSquared(anova1, anova = TRUE)
  expect_true(is.matrix(result))
  expect_equal(colnames(result), c("eta.sq", "eta.sq.part", "SS", "df", "MS", "F", "p"))
})

test_that("etaSquared errors on invalid type", {
  expect_error(etaSquared(anova1, type = 4), "type must be equal to 1,2 or 3")
  expect_error(etaSquared(anova1, type = "II"), "type must be equal to 1,2 or 3")
})

test_that("etaSquared errors when x is not a linear model", {
  expect_error(etaSquared(list(a = 1)), '"x" must be a linear model object')
  expect_error(etaSquared("not a model"), '"x" must be a linear model object')
})

test_that("etaSquared errors on invalid anova argument", {
  expect_error(etaSquared(anova1, anova = "yes"))
  expect_error(etaSquared(anova1, anova = NA))
})
