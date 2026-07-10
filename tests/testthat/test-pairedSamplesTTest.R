# Long-form data frame
df1 <- data.frame(
  id   = factor(x = c(1,1,2,2,3,3,4,4), labels = c("alice","bob","chris","diana")),
  time = factor(x = c(1,2,1,2,1,2,1,2), labels = c("time1","time2")),
  wm   = c(3, 4, 6, 6, 9, 12, 7, 9)
)

# Wide form
df2 <- longToWide(df1, wm ~ time)

# Missing data: NA value
df3      <- df1
df3$wm[1] <- NA

# Missing data: missing case
df4 <- df1[-1, ]

test_that("pairedSamplesTTest examples work", {
  tt1 <- pairedSamplesTTest(formula = wm ~ time, data = df1, id = "id")
  tt2 <- pairedSamplesTTest(formula = wm ~ time + (id), data = df1)
  tt3 <- pairedSamplesTTest(formula = ~wm_time1 + wm_time2, data = df2)

  expect_s3_class(tt1, "TTest")
  expect_s3_class(tt2, "TTest")
  expect_s3_class(tt3, "TTest")

  expect_equal(tt1, tt2)
  for (n in names(tt1)) {
    if (!n %in% c("outcome","group","group.names","id")) {
      expect_equal(tt1[[n]], tt3[[n]])
    }
  }

  tt4 <- pairedSamplesTTest(formula = wm ~ time, data = df1, id = "id", one.sided = "time2")
  expect_s3_class(tt4, "TTest")

  expect_warning(pairedSamplesTTest(formula = wm ~ time, data = df3, id = "id"),
                 "removed due to missingness")
  expect_warning(pairedSamplesTTest(formula = wm ~ time, data = df4, id = "id"),
                 "removed due to missingness")
  expect_equal(
    suppressWarnings(pairedSamplesTTest(formula = wm ~ time, data = df3, id = "id")),
    suppressWarnings(pairedSamplesTTest(formula = wm ~ time, data = df4, id = "id"))
  )
})

test_that("pairedSamplesTTest t-statistic and p-value match t.test (paired)", {
  base <- t.test(df2$wm_time1, df2$wm_time2, paired = TRUE)
  tt   <- pairedSamplesTTest(formula = ~wm_time1 + wm_time2, data = df2)
  expect_equal(unname(tt$t.statistic), unname(base$statistic), tolerance = 1e-6)
  expect_equal(tt$p.value,             base$p.value,           tolerance = 1e-6)
  expect_equal(tt$conf.int,            unname(base$conf.int),  tolerance = 1e-6)
})

test_that("pairedSamplesTTest group means match observed means", {
  tt <- pairedSamplesTTest(formula = ~wm_time1 + wm_time2, data = df2)
  expect_equal(unname(tt$mean[1]), mean(df2$wm_time1), tolerance = 1e-10)
  expect_equal(unname(tt$mean[2]), mean(df2$wm_time2), tolerance = 1e-10)
})

test_that("pairedSamplesTTest effect size equals cohensD (paired method)", {
  tt <- pairedSamplesTTest(formula = ~wm_time1 + wm_time2, data = df2)
  expect_equal(
    tt$effect.size,
    cohensD(x = df2$wm_time1, y = df2$wm_time2, method = "paired"),
    tolerance = 1e-6
  )
})

test_that("pairedSamplesTTest works when data is a tibble (long form)", {
  skip_if_not_installed("tibble")
  tbl1 <- tibble::as_tibble(df1)
  tt_df  <- pairedSamplesTTest(wm ~ time, df1,  id = "id")
  tt_tbl <- pairedSamplesTTest(wm ~ time, tbl1, id = "id")
  expect_equal(tt_df$t.statistic, tt_tbl$t.statistic, tolerance = 1e-6)
  expect_equal(tt_df$p.value,     tt_tbl$p.value,     tolerance = 1e-6)
  expect_equal(tt_df$conf.int,    tt_tbl$conf.int,    tolerance = 1e-6)
})

test_that("pairedSamplesTTest works when data is a tibble (wide form)", {
  skip_if_not_installed("tibble")
  tbl2 <- tibble::as_tibble(df2)
  tt_df  <- pairedSamplesTTest(~wm_time1 + wm_time2, df2)
  tt_tbl <- pairedSamplesTTest(~wm_time1 + wm_time2, tbl2)
  expect_equal(tt_df$t.statistic, tt_tbl$t.statistic, tolerance = 1e-6)
  expect_equal(tt_df$p.value,     tt_tbl$p.value,     tolerance = 1e-6)
  expect_equal(tt_df$conf.int,    tt_tbl$conf.int,    tolerance = 1e-6)
})

test_that("pairedSamplesTTest errors on invalid conf.level", {
  expect_error(pairedSamplesTTest(wm ~ time, df1, id = "id", conf.level = c(0.9, 0.95)),
               '"conf.level" must be a number between 0 and 1')
  expect_error(pairedSamplesTTest(wm ~ time, df1, id = "id", conf.level = NA_real_),
               '"conf.level" must be a number between 0 and 1')
  expect_error(pairedSamplesTTest(wm ~ time, df1, id = "id", conf.level = 1.5),
               '"conf.level" must be a number between 0 and 1')
  expect_error(pairedSamplesTTest(wm ~ time, df1, id = "id", conf.level = "high"),
               '"conf.level" must be a number between 0 and 1')
})
