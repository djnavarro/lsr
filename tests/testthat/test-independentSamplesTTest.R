df <- data.frame(
  rt   = c(451, 562, 704, 324, 505, 600, 829),
  cond = factor(x = c(1,1,1,2,2,2,2), labels = c("group1","group2"))
)

df2 <- df
df2$rt[1]   <- NA
df2$cond[7] <- NA

test_that("independentSamplesTTest examples work", {
  expect_s3_class(independentSamplesTTest(rt ~ cond, df), "TTest")
  expect_s3_class(independentSamplesTTest(rt ~ cond, df, var.equal = TRUE), "TTest")
  expect_s3_class(independentSamplesTTest(rt ~ cond, df, one.sided = "group1"), "TTest")
  expect_s3_class(suppressWarnings(independentSamplesTTest(rt ~ cond, df2)), "TTest")
  expect_warning(independentSamplesTTest(rt ~ cond, df2), "removed due to missingness")

  tt <- list(
    welch   = independentSamplesTTest(rt ~ cond, df),
    student = independentSamplesTTest(rt ~ cond, df, var.equal = TRUE),
    oneside = independentSamplesTTest(rt ~ cond, df, one.sided = "group1"),
    missing = suppressWarnings(independentSamplesTTest(rt ~ cond, df2))
  )

  for (test in 1:4) {
    expect_length(tt[[test]], 15)
    expect_named(
      tt[[test]],
      c("t.statistic","df","p.value","conf.int","conf","mean","sd","outcome",
        "group","group.names","id","mu","alternative","method","effect.size")
    )
    expect_equal(
      vapply(tt[[test]], length, numeric(1)),
      structure(
        c(1,1,1,2,1,2,2,1,1,2,0,0,1,1,1),
        .Names = c("t.statistic","df","p.value","conf.int","conf","mean","sd",
                   "outcome","group","group.names","id","mu","alternative",
                   "method","effect.size"))
    )
  }
})

test_that("independentSamplesTTest t-statistic and p-value match t.test (Welch)", {
  base  <- t.test(rt ~ cond, data = df)
  tt    <- independentSamplesTTest(rt ~ cond, df)
  expect_equal(unname(tt$t.statistic), unname(base$statistic), tolerance = 1e-6)
  expect_equal(tt$p.value,             base$p.value,           tolerance = 1e-6)
  expect_equal(tt$conf.int,            unname(base$conf.int),  tolerance = 1e-6)
})

test_that("independentSamplesTTest t-statistic and p-value match t.test (Student)", {
  base  <- t.test(rt ~ cond, data = df, var.equal = TRUE)
  tt    <- independentSamplesTTest(rt ~ cond, df, var.equal = TRUE)
  expect_equal(unname(tt$t.statistic), unname(base$statistic), tolerance = 1e-6)
  expect_equal(tt$p.value,             base$p.value,           tolerance = 1e-6)
})

test_that("independentSamplesTTest effect size matches cohensD with method = unequal", {
  # var.equal = FALSE (default) → cohensD is called with method = "unequal"
  tt <- independentSamplesTTest(rt ~ cond, df)
  expect_equal(
    tt$effect.size,
    cohensD(rt ~ cond, data = df, method = "unequal"),
    tolerance = 1e-6
  )
})

test_that("independentSamplesTTest group means match tapply result", {
  tt  <- independentSamplesTTest(rt ~ cond, df)
  ref <- tapply(df$rt, df$cond, mean)
  # tapply returns a named array (has dim); convert to plain vector for comparison
  expect_equal(unname(tt$mean), as.vector(ref), tolerance = 1e-10)
})

test_that("independentSamplesTTest works when data is a tibble", {
  skip_if_not_installed("tibble")
  dft <- tibble::as_tibble(df)
  tt_df  <- independentSamplesTTest(rt ~ cond, df)
  tt_tbl <- independentSamplesTTest(rt ~ cond, dft)
  expect_equal(tt_df$t.statistic, tt_tbl$t.statistic, tolerance = 1e-6)
  expect_equal(tt_df$p.value,     tt_tbl$p.value,     tolerance = 1e-6)
  expect_equal(tt_df$conf.int,    tt_tbl$conf.int,    tolerance = 1e-6)
})

test_that("independentSamplesTTest errors on invalid conf.level", {
  expect_error(independentSamplesTTest(rt ~ cond, df, conf.level = c(0.9, 0.95)),
               '"conf.level" must be a number between 0 and 1')
  expect_error(independentSamplesTTest(rt ~ cond, df, conf.level = NA_real_),
               '"conf.level" must be a number between 0 and 1')
  expect_error(independentSamplesTTest(rt ~ cond, df, conf.level = 1.5),
               '"conf.level" must be a number between 0 and 1')
  expect_error(independentSamplesTTest(rt ~ cond, df, conf.level = "high"),
               '"conf.level" must be a number between 0 and 1')
})
