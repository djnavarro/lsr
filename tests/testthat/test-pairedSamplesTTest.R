

# long form data frame
df1 <- data.frame(
  id = factor( x=c(1, 1, 2, 2, 3, 3, 4, 4),
               labels=c("alice","bob","chris","diana") ),
  time = factor( x=c(1,2,1,2,1,2,1,2),
                 labels=c("time1","time2")),
  wm = c(3, 4, 6, 6, 9, 12,7,9)
)

# wide form
df2 <- longToWide(df1, wm ~ time)

# missing data because of NA values
df3 <- df1
df3$wm[1] <- NA

# missing data because of missing cases from the long form data frame
df4 <- df1
df4 <- df4[-1,]

test_that("pairedSamplesTTest examples work", {

  # basic test, run from long form or wide form data
  tt1 <- pairedSamplesTTest(formula = wm ~ time, data = df1, id = "id")
  tt2 <- pairedSamplesTTest(formula = wm ~ time + (id), data = df1)
  tt3 <- pairedSamplesTTest(formula = ~wm_time1 + wm_time2, data = df2)

  expect_s3_class(tt1, "TTest")
  expect_s3_class(tt2, "TTest")
  expect_s3_class(tt3, "TTest")

  expect_equal(tt1, tt2)
  for(n in names(tt1)) {
    if(!n %in% c("outcome", "group", "group.names", "id")) {
      expect_equal(tt1[[n]], tt3[[n]])
    }
  }

  tt4 <- pairedSamplesTTest(formula= wm~time, data=df1, id="id", one.sided="time2")
  expect_s3_class(tt4, "TTest")

  expect_warning(
    pairedSamplesTTest(formula = wm~time, data=df3, id="id"),
    "removed due to missingness"
  )
  expect_warning(
    pairedSamplesTTest(formula = wm~time, data=df4, id="id"),
    "removed due to missingness"
  )

  expect_equal(
    suppressWarnings(pairedSamplesTTest(formula = wm~time, data=df3, id="id")),
    suppressWarnings(pairedSamplesTTest(formula = wm~time, data=df4, id="id"))
  )
})
