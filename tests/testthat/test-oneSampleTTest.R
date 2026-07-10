likert1 <- c(3, 1, 4, 1, 4, 6, 7, 2, 6, 6, 7)
likert2 <- c(3, NA, 4, NA, 4, 6, 7, NA, 6, 6, 7)

test_that("oneSampleTTest structure is correct", {
  tt <- list(
    oneSampleTTest(x = likert1, mu = 4),
    oneSampleTTest(x = likert1, mu = 4, one.sided = "greater"),
    oneSampleTTest(x = likert1, mu = 4, conf.level = .99),
    suppressWarnings(oneSampleTTest(x = likert2, mu = 4))
  )

  expect_warning(oneSampleTTest(x = likert2, mu = 4), "removed due to missingness")

  for (test in 1:length(tt)) {
    expect_s3_class(tt[[test]], "TTest")
    expect_length(tt[[test]], 15)
    expect_named(
      tt[[test]],
      c("t.statistic","df","p.value","conf.int","conf","mean","sd","outcome",
        "group","group.names","id","mu","alternative","method","effect.size")
    )
    expect_equal(
      vapply(tt[[test]], length, numeric(1)),
      structure(
        c(1,1,1,2,1,1,1,1,0,0,0,1,1,1,1),
        .Names = c("t.statistic","df","p.value","conf.int","conf","mean","sd",
                   "outcome","group","group.names","id","mu","alternative",
                   "method","effect.size"))
    )
  }
})

test_that("oneSampleTTest t-statistic and p-value match t.test", {
  base <- t.test(likert1, mu = 4)
  tt   <- oneSampleTTest(x = likert1, mu = 4)
  expect_equal(unname(tt$t.statistic), unname(base$statistic), tolerance = 1e-6)
  expect_equal(tt$p.value,             base$p.value,           tolerance = 1e-6)
  expect_equal(tt$conf.int,            unname(base$conf.int),  tolerance = 1e-6)
})

test_that("oneSampleTTest one-sided p-value matches t.test (alternative = greater)", {
  base <- t.test(likert1, mu = 4, alternative = "greater")
  tt   <- oneSampleTTest(x = likert1, mu = 4, one.sided = "greater")
  expect_equal(tt$p.value, base$p.value, tolerance = 1e-6)
})

test_that("oneSampleTTest mean matches mean(x)", {
  tt <- oneSampleTTest(x = likert1, mu = 4)
  expect_equal(tt$mean, mean(likert1), tolerance = 1e-10)
})

test_that("oneSampleTTest effect size equals cohensD for one-sample case", {
  tt <- oneSampleTTest(x = likert1, mu = 4)
  expect_equal(tt$effect.size, cohensD(x = likert1, mu = 4), tolerance = 1e-6)
})

test_that("oneSampleTTest errors on invalid conf.level", {
  expect_error(oneSampleTTest(likert1, mu = 4, conf.level = c(0.9, 0.95)),
               '"conf.level" must be a number between 0 and 1')
  expect_error(oneSampleTTest(likert1, mu = 4, conf.level = NA_real_),
               '"conf.level" must be a number between 0 and 1')
  expect_error(oneSampleTTest(likert1, mu = 4, conf.level = 1.5),
               '"conf.level" must be a number between 0 and 1')
  expect_error(oneSampleTTest(likert1, mu = 4, conf.level = "high"),
               '"conf.level" must be a number between 0 and 1')
})
