long <- data.frame(
  id       = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  time     = c("t1","t1","t1","t2","t2","t2","t3","t3","t3"),
  accuracy = c(.50, .03, .72, .94, .63, .49, .78, .71, .16)
)

test_that("longToWide returns a data frame with expected structure", {
  # Regression guard for original structural tests
  result <- longToWide(long, accuracy ~ time)
  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(3L, 4L))
  expect_named(result, c("id", "accuracy_t1", "accuracy_t2", "accuracy_t3"))
})

test_that("longToWide preserves cell values correctly", {
  result <- longToWide(long, accuracy ~ time)
  # participant 1: t1=.50, t2=.94, t3=.78
  expect_equal(result$accuracy_t1[result$id == 1], 0.50)
  expect_equal(result$accuracy_t2[result$id == 1], 0.94)
  expect_equal(result$accuracy_t3[result$id == 1], 0.78)
  # participant 2: t1=.03, t2=.63, t3=.71
  expect_equal(result$accuracy_t1[result$id == 2], 0.03)
  expect_equal(result$accuracy_t2[result$id == 2], 0.63)
  expect_equal(result$accuracy_t3[result$id == 2], 0.71)
})

test_that("longToWide custom sep argument is respected", {
  result <- longToWide(long, accuracy ~ time, sep = ".")
  expect_named(result, c("id", "accuracy.t1", "accuracy.t2", "accuracy.t3"))
})

test_that("longToWide round-trips through wideToLong for simple cases", {
  wide <- longToWide(long, accuracy ~ time)
  # Convert back to long form and check values are preserved
  back <- wideToLong(wide, "time")
  # Same number of rows and the accuracy values match (order may differ)
  expect_equal(nrow(back), nrow(long))
  # Merge on id + time and verify accuracy values agree
  merged <- merge(long, back, by = c("id", "time"), suffixes = c(".orig", ".rt"))
  expect_equal(merged$accuracy.orig, merged$accuracy.rt, tolerance = 1e-10)
})

test_that("longToWide errors on invalid inputs", {
  expect_error(longToWide(list(a = 1:3), accuracy ~ time), '"data" must be a data frame')
  expect_error(longToWide(long, "accuracy ~ time"),         '"formula" must be a formula')
  expect_error(longToWide(long, ~accuracy),                 '"formula" must be a two-sided formula')
  expect_error(longToWide(long, accuracy ~ time, sep = 1),  '"sep" must be a single character string')
  expect_error(longToWide(long, accuracy ~ time, sep = c("_","-")), '"sep" must be a single character string')
})

test_that("longToWide works with multiple within-subject variables", {
  long2 <- data.frame(
    id      = c(1,1,1,1, 2,2,2,2),
    day     = c("d1","d1","d2","d2", "d1","d1","d2","d2"),
    session = c("s1","s2","s1","s2", "s1","s2","s1","s2"),
    score   = c(10, 20, 30, 40, 50, 60, 70, 80)
  )
  result <- longToWide(long2, score ~ day + session)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  # Should produce 4 score columns, one per day x session combination
  score_cols <- grep("^score_", names(result), value = TRUE)
  expect_length(score_cols, 4L)
})
