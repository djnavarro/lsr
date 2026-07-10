test_that("simple cohensD use cases work", {

  # one sample
  expect_equal(cohensD(x = 1:10), 1.81659, tolerance = .001)

  # independent samples
  expect_equal(cohensD(x = 1:10, y = 0:9), 0.3302891, tolerance = .001)

  # independent samples, formula version
  df <- data.frame(
    val = c(1:10, 0:9),
    grp = c(rep(1, 10), rep(2, 10))
  )
  expect_equal(cohensD(val ~ grp, data = df), 0.3302891, tolerance = .001)
})

test_that("cohensD errors on character input", {

  chr <- c("a","b", "c")
  num <- 1:3
  df <- data.frame(
    chr = letters[1:20],
    grp = c(rep(1, 10), rep(2, 10))
  )

  expect_error(cohensD(x = chr))
  expect_error(cohensD(x = num, y = chr))

  # this doesn't error on R <= 3.6
  # expect_error(suppressWarnings(cohensD(chr ~ grp, data = df)))

})


test_that("different cohensD methods return expected numbers", {

  x <- c(1, 2, 3, 4, 5, 6)
  y <- c(2, 1, 0, 4, 6, 3)

  expect_equal(cohensD(x, y, method = "pooled"),  0.412393,  tolerance = .0001)
  expect_equal(cohensD(x, y, method = "x.sd"),    0.4454354, tolerance = .0001)
  expect_equal(cohensD(x, y, method = "y.sd"),    0.3857584, tolerance = .0001)
  expect_equal(cohensD(x, y, method = "raw"),     0.451754,  tolerance = .0001)
  expect_equal(cohensD(x, y, method = "paired"),  0.4541703, tolerance = .0001)
  expect_equal(cohensD(x, y, method = "unequal"), 0.412393,  tolerance = .0001) # hm? same as pooled? really?

})

test_that("cohensD corrected method applies the bias-correction factor", {
  x <- c(1, 2, 3, 4, 5, 6)
  y <- c(2, 1, 0, 4, 6, 3)
  n <- length(x) + length(y)
  expect_equal(
    cohensD(x, y, method = "corrected"),
    cohensD(x, y, method = "pooled") * (n - 3) / (n - 2.25),
    tolerance = 1e-6
  )
})

test_that("cohensD formula= alias gives the same result as x= formula", {
  grade   <- c(55, 65, 65, 68, 70, 56, 60, 62, 66)
  teacher <- c("A", "A", "A", "A", "A", "B", "B", "B", "B")
  exams   <- data.frame(grade, teacher)
  expect_equal(
    cohensD(formula = grade ~ teacher, data = exams),
    cohensD(x = grade ~ teacher, data = exams),
    tolerance = 1e-6
  )
})

test_that("cohensD warns for method = 'paired' with formula input", {
  # equal group sizes so the function completes after issuing the warning
  grade   <- c(55, 65, 65, 68, 70, 56, 60, 62, 66, 71)
  teacher <- c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")
  exams   <- data.frame(grade, teacher)
  expect_warning(
    cohensD(grade ~ teacher, data = exams, method = "paired"),
    "Results will be incorrect"
  )
})

test_that("cohensD errors on invalid method", {
  x <- c(1, 2, 3, 4, 5, 6)
  y <- c(2, 1, 0, 4, 6, 3)
  expect_error(cohensD(x, y, method = "bad"), '"method" must be')
})

test_that("cohensD errors when grouping factor has more than two levels", {
  grade   <- c(55, 65, 65, 68, 70, 56, 60, 62, 66)
  teacher <- c("A", "A", "A", "B", "B", "B", "C", "C", "C")
  exams   <- data.frame(grade, teacher)
  expect_error(cohensD(grade ~ teacher, data = exams), "exactly 2 levels")
})


