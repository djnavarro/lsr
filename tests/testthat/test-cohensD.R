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


