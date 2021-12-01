
likert1 <- c(3,1,4,1,4,6,7,2,6,6,7)
likert2 <- c(3,NA,4,NA,4,6,7,NA,6,6,7)


test_that("oneSampleTTest", {

  tt <- list(
    oneSampleTTest(x = likert1, mu = 4),
    oneSampleTTest(x = likert1, mu = 4, one.sided = "greater"),
    oneSampleTTest(x = likert1, mu = 4, conf.level=.99),
    suppressWarnings(oneSampleTTest(x = likert2, mu = 4))
  )

  expect_warning(
    oneSampleTTest(x = likert2, mu = 4),
    "removed due to missingness"
  )

  for(test in 1:length(tt)) {

    expect_s3_class(tt[[test]], "TTest")
    expect_length(tt[[test]], 15)
    expect_named(
      tt[[test]],
      c("t.statistic", "df", "p.value", "conf.int", "conf", "mean",
        "sd", "outcome", "group", "group.names", "id", "mu", "alternative",
        "method", "effect.size")
    )
    expect_equal(
      vapply(tt[[test]], length, numeric(1)),
      structure(
        c(1, 1, 1, 2, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
        .Names = c("t.statistic",
                   "df", "p.value", "conf.int", "conf", "mean", "sd", "outcome",
                   "group", "group.names", "id", "mu", "alternative", "method",
                   "effect.size"))

    )

  }

})
