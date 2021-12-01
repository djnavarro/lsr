df <- data.frame(
  rt = c(451, 562, 704, 324, 505, 600, 829),
  cond = factor(
    x=c(1,1,1,2,2,2,2),
    labels=c("group1","group2")
  )
)

df2 <- df
df2$rt[1] <- NA
df2$cond[7] <- NA

test_that("independentSamplesTTest examples work", {

  expect_s3_class(independentSamplesTTest(rt ~ cond, df), "TTest")
  expect_s3_class(independentSamplesTTest(rt ~ cond, df, var.equal=TRUE), "TTest")
  expect_s3_class(independentSamplesTTest(rt ~ cond, df, one.sided = "group1"), "TTest")
  expect_s3_class(suppressWarnings(independentSamplesTTest(rt ~ cond, df2)), "TTest")

  expect_warning(independentSamplesTTest(rt ~ cond, df2), "removed due to missingness")

  tt <- list(
    welch   = independentSamplesTTest(rt ~ cond, df),
    student = independentSamplesTTest(rt ~ cond, df, var.equal=TRUE),
    oneside = independentSamplesTTest(rt ~ cond, df, one.sided = "group1"),
    missing = suppressWarnings(independentSamplesTTest(rt ~ cond, df2))
  )

  for(test in 1:4) {
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
        c(1, 1, 1, 2, 1, 2, 2, 1, 1, 2, 0, 0, 1, 1, 1),
        .Names = c("t.statistic",
                   "df", "p.value", "conf.int", "conf", "mean", "sd", "outcome",
                   "group", "group.names", "id", "mu", "alternative", "method",
                   "effect.size"))

    )
  }

})
